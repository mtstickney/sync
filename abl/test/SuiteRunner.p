{client/include/shecl/shecl.i}
{client/include/system.i}

DEFINE INPUT PARAMETER binDir AS CHARACTER NO-UNDO.

DEFINE VAR suites AS CHARACTER EXTENT
        INITIAL [
        "ObjPoolSuite.p",
        "IntegerConversionSuite.p",
        "Int64ConversionSuite.p",
        "StringConversionSuite.p",
        "DecimalConversionSuite.p",
        "LogicalConversionSuite.p",
        "GeneralizedBooleanConversionSuite.p",
        "ReadSuite.p",
        "CallSuite.p",
        "EvalSuite.p"]
        NO-UNDO.

DEFINE VAR suite AS CHARACTER NO-UNDO.
DEFINE VAR result AS LOGICAL NO-UNDO.
DEFINE VAR hdl AS HANDLE NO-UNDO.
DEFINE VAR i AS INTEGER NO-UNDO.
DEFINE VAR passed AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VAR path AS CHARACTER NO-UNDO.

path = DirectoryOf(binDir).
AddToPath(path + ";" + path + "\ecl\").

/* Boot the system. */
SheclInit().

DO i = 1 TO EXTENT(suites):
        suite = suites[i].
        RUN VALUE(suite) PERSISTENT SET hdl.
        RUN RunSuite IN hdl (SUBSTRING(suite, 1, LENGTH(suite) - 2), binDir, OUTPUT result).
        DELETE PROCEDURE hdl.
        IF result = YES THEN
                passed = passed + 1.
END.

SheclShutdown().

PUT SKIP EXTENT(suites) " suites, " passed " passed, " EXTENT(suites) - passed " failed." SKIP(1).
RETURN.