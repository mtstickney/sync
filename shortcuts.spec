# -*- mode: python -*-

import os
from os.path import *

from PyInstaller.utils.hooks import *

block_cipher = None

_NAME = "convert_shortcuts"
_NDEBUG = True

cwd = os.getcwd()
a = Analysis(
    ['convert_shortcuts.py'],
    pathex=[cwd],
    binaries=[],
    datas=[
    ],
    hiddenimports=[],
    hookspath=[],
    runtime_hooks=[],
    excludes=[],
    win_no_prefer_redirects=False,
    win_private_assemblies=False,
    cipher=block_cipher
)

pyz = PYZ(
    a.pure,
    a.zipped_data,
    cipher=block_cipher
)

exe = EXE(
    pyz,
    a.scripts,
    a.binaries,
    a.zipfiles,
    a.datas,
    name=_NAME,
    debug= not _NDEBUG,
    strip=False,
    upx=True,
    console=True)

# uncomment this to build-as-directory
"""
coll = COLLECT(
    exe,
    a.binaries,
    a.zipfiles,
    a.datas,
    strip=False,
    upx=True,
    name=_NAME
)
"""