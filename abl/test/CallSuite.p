{client/include/shecl/shecl.i}

{test.i "Call1SymbolWithEmptyArglist"}
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR func AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        pool = MakeObjPool().
        func = Read(pool, "+").
        obj = Call1(pool, func, Nil()).
        ret = ABLInteger(obj).
        ReleaseObjPool(pool).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = 0.
END.

{test.i "Call1LambdaWithEmptyArglist"}
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR func AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        pool = MakeObjPool().
        func = Eval(pool, "(lambda () 17)").
        obj = Call1(pool, func, Nil()).
        ret = ABLInteger(obj).
        ReleaseObjPool(pool).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = 17.
END.

{test.i "Call1SymbolWithArgs"}
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR func AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR args AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        pool = MakeObjPool().
        func = Read(pool, "+").
        args = Read(pool, "(1 2 3 4)").
        obj = Call1(pool, func, args).
        ret = ABLInteger(obj).
        ReleaseObjPool(pool).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = 10.
END.

{test.i "Call1LambdaWithArgs"}
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR func AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR args AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        pool = MakeObjPool().
        func = Eval(pool, "(lambda (a b c d) (- a b c d))").
        args = Read(pool, "(1 2 3 4)").
        obj = Call1(pool, func, args).
        ret = ABLInteger(obj).
        ReleaseObjPool(pool).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = -8.
END.

{client/include/shecl/call.i 2}

{test.i "Call2SymbolWithArgs"}
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR func AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR args AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        pool = MakeObjPool().
        func = Read(pool, "+").
        args = Read(pool, "(2 3 4)").
        obj = Call2(pool, func, Read(pool, "1"), args).
        ret = ABLInteger(obj).
        ReleaseObjPool(pool).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = 10.
END.

{test.i "Call2LambdaWithArgs"}
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR func AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR args AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        pool = MakeObjPool().
        func = Eval(pool, "(lambda (a b c d) (- a b c d))").
        args = Read(pool, "(2 3 4)").
        obj = Call2(pool, func, Read(pool, "1"), args).
        ret = ABLInteger(obj).
        ReleaseObjPool(pool).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = -8.
END.

{suiterunner.i}
