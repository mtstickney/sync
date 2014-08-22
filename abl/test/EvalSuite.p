{client/include/shecl/shecl.i}

{test.i "EvalError"}
        DEFINE VAR ret AS {&CLOBJECT} NO-UNDO.

        Eval({&CLNULL}, '(error "Error message")').
        REtURN ERROR-STATUS:ERROR.
END.

{test.i "EvalNumber"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        obj = Eval({&CLNULL}, "43").
        ret = ABLInteger(obj).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = 43.
END.

{test.i "EvalString"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS CHARACTER NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.

        pool = MakeObjPool().
        obj = Eval(pool, '"flub"').
        ret = ABLString(obj).
        ReleaseObjPool(pool).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = "flub".
END.

{test.i "EvalExpression"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.

        pool = MakeObjPool().
        obj = Eval(pool, "(+ 4 5)").
        ret = ABLInteger(obj).
        ReleaseObjPool(pool).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = 9.
END.

{test.i "EvalWithRead"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.

        pool = MakeObjPool().
        obj = Eval(pool, "(+ #.(- 6 2) 5)").
        ret = ABLInteger(obj).
        ReleaseObjPool(pool).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = 9.
END.

{test.i "EvalSpecialForm"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        obj = Eval({&CLNULL}, "(let ((a 3) (b 4)) (+ a b))").
        ret = ABLInteger(obj).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = 7.
END.

{suiterunner.i}
