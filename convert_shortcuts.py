from win32com.shell import shell
from win32com.taskscheduler import taskscheduler
import pythoncom
import sys
from ctypes import *
from uuid import UUID

# Type definitions
dword = c_ulong

class WinBool:
    def __init__(self, val=None):
        self._as_parameter_ = c_int(1 if val else 0)

    def __call__(self, val):
        if val != 0:
            return True
        return False

class NetUserFilter:
    FILTER_TEMP_DUPLICATE_ACCOUNT = 0x01
    FILTER_NORMAL_ACCOUNT = 0x02
    FILTER_INTERDOMAIN_TRUST_ACCOUNT = 0x08
    FILTER_WORKSTATION_TRUST_ACCOUNT = 0x10
    FILTER_SERVER_TRUST_ACCOUNT = 0x20

class Err:
    NERR_Success = 0
    NERR_Base = 2100
    NERR_BufTooSmall = NERR_Base + 23
    NERR_InvalidComputer = NERR_Base + 251

    ERROR_ACCESS_DENIED = 5
    ERROR_INVALID_LEVEL = 124
    ERROR_MORE_DATA = 234

MAX_PREFERRED_LENGTH = -1

class USER_INFO_0 (Structure):
    _fields_ = [("name", c_wchar_p)]

USER_INFO_0_PTR = POINTER(USER_INFO_0)

netapi32 = windll.Netapi32
net_user_enum = netapi32.NetUserEnum
net_user_enum.argtypes = [
    c_wchar_p,
    dword,
    dword,
    POINTER(USER_INFO_0_PTR),
    dword,
    POINTER(dword),
    POINTER(dword),
    POINTER(dword)
]
net_user_enum.restype = dword

net_api_buffer_free = netapi32.NetApiBufferFree
net_api_buffer_free.argtypes = [c_void_p]
net_api_buffer_free.restyep = dword

class WTSConfigClass:
    InitialProgram = 0
    WorkingDirectory = 1
    InheritInitialProgram = 2
    
wtsapi32 = windll.Wtsapi32
wts_query_user_config = wtsapi32.WTSQueryUserConfigW
wts_query_user_config.argtypes = [c_wchar_p, c_wchar_p, c_int, POINTER(c_wchar_p), POINTER(dword)]
wts_query_user_config.restype = WinBool()

wts_free_memory = wtsapi32.WTSFreeMemory
wts_free_memory.argtypes = [c_void_p]
wts_free_memory.restype = None

def user_list(machine_name):
    entries_read = dword()
    total_entries = dword()
    resume_handle = dword(0)
    resume_ptr = pointer(resume_handle)
    bufp = USER_INFO_0_PTR()
    names = []

    while True:
        status = net_user_enum(
            machine_name, # NULL for the local machine.
            dword(0), # Names only
            dword(NetUserFilter.FILTER_NORMAL_ACCOUNT),
            byref(bufp),
            dword(MAX_PREFERRED_LENGTH),
            byref(entries_read),
            byref(total_entries),
            byref(resume_handle)
        )

        for i in range(entries_read.value):
            names.append(bufp[i].name)

        # Have to free the buffer, regardless of status.
        if status == Err.NERR_Success or status == Err.ERROR_MORE_DATA:
            # FIXME: ignoring possible error.
            net_api_buffer_free(bufp)

        if status == Err.NERR_Success:
            break
        elif status == Err.ERROR_MORE_DATA:
            print("More entries to read")
        else:
            print("Ruh-roh: {}".format(status))
            break
    return names

def user_start_program(name):
    data_size = dword()
    value = c_wchar_p()
    
    if not wts_query_user_config(None, name, WTSConfigClass.InitialProgram, byref(value), data_size):
        print("Aw, no dice")
        return None
    else:
        str = value.value
        wts_free_memory(cast(value, c_void_p))
        return str

def user_start_dir(name):
    data_size = dword()
    value = c_wchar_p()
    
    if not wts_query_user_config(None, name, WTSConfigClass.WorkingDirectory, byref(value), data_size):
        print("Aw, no dice")
        return None
    else:
        str = value.value
        wts_free_memory(cast(value, c_void_p))
        return str

def user_login_info(name):
    return (user_start_program(name), user_start_dir(name))

def shortcut_info(path):
    shortcut = pythoncom.CoCreateInstance(shell.CLSID_ShellLink, None, pythoncom.CLSCTX_INPROC_SERVER, shell.IID_IShellLink)
    pfile = shortcut.QueryInterface(pythoncom.IID_IPersistFile)

    pfile.Load(sys.argv[1])
    return (shortcut.GetPath(shell.SLGP_RAWPATH), shortcut.GetArguments(), shortcut.GetIconLocation())

# Technically a pointer to a struct, but meh. We have bytes instead.
REFIID = POINTER(c_char)

class InterfaceStruct (Structure):
    _fields_ = [("vtable", c_void_p)]

Interface = POINTER(InterfaceStruct)

hresult = c_long
class HResult:
    def __init__(self, val=None):
        if val is not None:
            self._as_parameter_ = hresult(val)

    def __call__(self, val):
        if val != 0:
            raise OSError(val)

VARTYPE = c_ushort
class VarEnum:
    VT_EMPTY = VARTYPE(0)
    VT_NULL = VARTYPE(1)

class BRecord (Structure):
    _fields_ = [("record", c_void_p), ("irecordinfo", Interface)]

class VariantUnion (Union):
    _fields = [("llVal", c_longlong), ("pllVal", POINTER(c_longlong)), ("brecord", BRecord)]

class VariantTagStruct (Structure):
    _fields_ = [
        ("vt", VARTYPE),
        ("reserved1", c_short),
        ("reserved2", c_short),
        ("reserved3", c_short),
        ("value", VariantUnion)]

class Variant (Structure):
    # Note that the real definition has at least two extra levels in
    # here, but fuck that noise.
    _fields_ = [("variantData", VariantTagStruct)]

class BStr:
    def __init__(self, str):
        # A BStr is a 4-byte length header and a utf-16 string.
        # 4 bytes == 2 utf-16 characters, so we'll pad, then scribble
        # over the string to create the length header, and then do 
        # sketchy pointer math to get a bstr. whee.
        self.c_val = c_wchar_p("XX" + str)
        lenp = cast(self.c_val, POINTER(c_ulong))
        lenp[0] = 2 * len(str) # NUL is not included in the length

        self._as_parameter_ = c_wchar_p.from_buffer(self.c_val, 4)

    @classmethod
    def from_param(self, obj):
        if isinstance(obj, BStr):
            return obj
        else:
            return self(obj)


def parse_uuid(uuid_str):
    if sys.byteorder == 'little':
        return UUID(uuid_str).bytes_le
    else:
        return UUID(uuid_str).bytes

class BaseInterface:
    def __init__(self, sap):
        self.sap = sap

    def GetSap(self):
        return self.sap

    def MethodPointer(self, index):
        vtable = cast(self.sap.contents.vtable, POINTER(c_void_p))
        return vtable[index]

class IUnknown(BaseInterface):
    ADD_REF = WINFUNCTYPE(c_ulong, Interface)
    RELEASE = WINFUNCTYPE(c_ulong, Interface)
    QUERY_INTERFACE = WINFUNCTYPE(HResult(), Interface, REFIID, POINTER(Interface))

    IID_IUnknown = parse_uuid("{00000000-0000-0000-C000-000000000046}")

    @classmethod
    def GetIID(klass):
        return klass.IID_IUnknown

    def QueryInterface(self, interface):
        func = cast(self.MethodPointer(0), QUERY_INTERFACE)
        sap = Interface()
        func(self.sap, interface.GetIID(), byref(sap))
        return interface.__init__(sap)

    def AddRef(self):
        func = cast(self.MethodPointer(1), ADD_REF)
        return func(self.sap)

    def Release(self):
        func = cast(self.MethodPointer(2), RELEASE)
        return func(self.sap)

    def __del__(self):
        self.Release()

CLSID_TaskScheduler = parse_uuid("{0f87369f-a4e5-4cfc-bd3e-73e6154572dd}")

class IDispatch (IUnknown):
    pass

class ITaskService (IDispatch):
    IID_ITaskService = parse_uuid("{2faba4c7-4da9-4013-9697-20cc3fd40f85}")

    CONNECT = WINFUNCTYPE(HResult(), Interface, Variant, Variant, Variant, Variant)
    GET_FOLDER = WINFUNCTYPE(HResult(), Interface, BStr, POINTER(Interface))

    @classmethod
    def GetIID(klass):
        return klass.IID_ITaskService

    def Connect(self):
        empty = Variant()
        empty.variantData.vt = VarEnum.VT_EMPTY
        func = cast(self.MethodPointer(7 + 3), CONNECT)
        # Passing empty values for server, user, domain, and password means
        # to use the local machine and the current user token.
        func(self.sap, empty, empty, empty, empty)

    def GetFolder(self, path):
        folder_sap = Interface()
        func = cast(self.MethodPointer(7 + 0), GET_FOLDER)
        func(self.sap, path, byref(folder_sap))
        return ITaskFolder(folder_sap)

def scheduled_tasks():
    ts = pythoncom.CoCreateInstance(taskscheduler.CLSID_CTaskScheduler, None, pythoncom.CLSCTX_INPROC_SERVER, taskscheduler.IID_ITaskScheduler)
    return [ts.Activate(name, taskscheduler.IID_ITask) for name in ts.Enum()]

class ScheduledTask:
    def __init__(self, task):
        self.task = task

    @property
    def program(self):
        self.task.GetApplicationName()

    @program.setter
    def set_program(self, value):
        self.task.SetApplicationName(value)

    @property
    def arguments():
        self.task.GetParameters()

    @arguments.setter
    def set_arguments(self, value):
        self.task.SetParameters(value)

if __name__ == '__main__':
    #sc_info = shortcut_info(sys.argv[1])
    #print("Shortcut path: {}".format(shortcut.GetPath(shell.SLGP_RAWPATH)))
    #print("Shortcut args: {}".format(shortcut.GetArguments()))
    #print("Icon location: {}".format(shortcut.GetIconLocation()))
    #shortcut.SetIconLocation("C:\\v10\\icons\\Compass.ico", 0)
    #shortcut.SetIconLocation('', 0)
    #pfile.Save(sys.argv[1], 1)

    for user in user_list(None): # None == local machine
        print("User: {}".format(user))
        print("Program: {}".format(user_start_program(user)))
        print("Directory: {}".format(user_start_dir(user)))
        print('')
    sys.exit(0)
