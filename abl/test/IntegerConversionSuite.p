{client/include/shecl/shecl.i}
{testdata.i}

{test.i "LispIntegerUnknownPoolNotNull"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispInteger(?, 4).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN obj <> {&CLNULL}.
END.

{test.i "LispIntegerNullPoolNotNull"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispInteger({&CLNULL}, 4).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN obj <> {&CLNULL}.
END.

{test.i "LispIntegerNonNullPoolNotNull"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.

        pool = MakeObjPool().
        /* Using MAXINT32 here prevents it from being an immediate,
         * and ensures we actually use the pool. */
        obj = LispInteger(pool, {&MAXINT32}).
        ReleaseObjPool(pool).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN obj <> {&CLNULL}.
END.

{test.i "LispIntegerOfUnknownError"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispInteger({&CLNULL}, ?).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "LispIntegerOfIntegerData"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        obj = LispInteger({&CLNULL}, 4).
        ret = ABLInteger(obj).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = 4.
END.

{test.i "LispIntegerPredOfUnknownError"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        ret = IsCLInteger(?).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "LispIntegerPredOfNull"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        ret = IsCLInteger({&CLNULL}).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = NO.
END.

{test.i "LispIntegerPredOfNonCoercibleType"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispInt64({&CLNULL}, {&MAXINT32} + 10).
        ret = IsCLInteger(obj).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = NO.
END.

{test.i "LispIntegerPredOfInteger"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispInteger({&CLNULL}, 4).
        ret = IsCLInteger(obj).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = YES.
END.

{test.i "ABLIntegerOfUnknownError"}
        DEFINE VAR ret AS INTEGER NO-UNDO.

        ret = ABLInteger(?).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLIntegerOfNullError"}
        DEFINE VAR ret AS INTEGER NO-UNDO.

        ret = ABLInteger({&CLNULL}).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLIntegerOfNonCoercibleTypeError"}
        DEFINE VAR ret AS INTEGER NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispInt64({&CLNULL}, {&MAXINT32} + 10).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        ret = ABLInteger(obj).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLIntegerOfInteger"}
        DEFINE VAR ret AS INTEGER NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispInteger({&CLNULL}, 4).
        ret = ABLInteger(obj).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = 4.
END.

{suiterunner.i}
