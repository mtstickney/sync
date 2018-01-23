from win32com.shell import shell
import pythoncom
import sys
from ctypes import *

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
