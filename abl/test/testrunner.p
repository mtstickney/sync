DEFINE VAR binDir AS CHARACTER NO-UNDO.
DEFINE VAR path AS CHARACTER NO-UNDO.
DEFINE VAR propathDirs AS CHARACTER NO-UNDO.
DEFINE VAR len AS INTEGER NO-UNDO.

/* Add the bin/ and bin/ecl/ directories to the path. */
binDir = ENTRY(1, SESSION:PARAMETER, ',').
len = LENGTH(binDir).

/* All but the first parameter are propath entries. */
propathDirs = SUBSTRING(SESSION:PARAMETER, len + 2).
IF propathDirs <> "" AND propathDirs <> ? THEN
        PROPATH = propathDirs + "," + PROPATH.

RUN SuiteRunner.p(binDir).
QUIT.
