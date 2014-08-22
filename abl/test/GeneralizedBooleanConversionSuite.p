{client/include/shecl/shecl.i}

{test.i "ABLGeneralizedLogicalNullError"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        ret = ABLGeneralizedLogical({&CLNULL}).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLGeneralizedLogicalUnknownError"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        ret = ABLGeneralizedLogical(?).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLGeneralizedLogicalNonBooleanTrue"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispInteger({&CLNULL}, 4).
        ret = ABLGeneralizedLogical(obj).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = YES.
END.

{test.i "ABLGeneralizedLogicalNil"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = Read({&CLNULL}, "nil").
        ret = ABLGeneralizedLogical(obj).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = NO.
END.

{suiterunner.i}
