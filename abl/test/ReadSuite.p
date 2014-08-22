{client/include/shecl/shecl.i}

{test.i "ReadNumber"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        obj = Read({&CLNULL}, "43").
        ret = ABLInteger(obj).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = 43.
END.

{test.i "ReadString"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS CHARACTER NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.

        pool = MakeObjPool().
        obj = Read(pool, '"flub"').
        ret = ABLString(obj).
        ReleaseObjPool(pool).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = "flub".
END.

{test.i "ReadWithEval"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        obj = Read({&CLNULL}, "#.(+ 6 3)").
        ret = ABLInteger(obj).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = 9.
END.

{suiterunner.i}
