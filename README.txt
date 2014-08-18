Building
--------
To build, run the command:
%ECL_DIR%\ecl-cc.bat --compile shecl.c %ECL_DIR%\ecl.lib -I%ECL_DIR% /D_EXPORT /LD /Fo%OUT_DIR%\shecl.dll

where "%ECL_DIR%" is the directory where ECL is installed. This should
produce a file shecl.dll, which is what you want.
