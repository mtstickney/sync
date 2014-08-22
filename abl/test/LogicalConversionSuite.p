{client/include/shecl/shecl.i}

{test.i "LispBoolUnknownPoolNotNull"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispBool({&CLNULL}, YES).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN obj <> {&CLNULL}.
END.

{test.i "LispBoolNullPoolNotNull"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispBool({&CLNULL}, YES).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN obj <> {&CLNULL}.
END.

{test.i "LispBoolNonNullPoolNotNull"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.

        pool = MakeObjPool().
        obj = LispBool(pool, YES).
        ReleaseObjPool(pool).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN obj <> {&CLNULL}.
END.

{test.i "LispBoolOfLogicalDataTrue"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        obj = LispBool({&CLNULL}, YES).
        ret = ABLLogical(obj).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = YES.
END.

{test.i "LispBoolOfLogicalDataFalse"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR nil AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        obj = LispBool({&CLNULL}, NO).
        ret = ABLLogical(obj).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = NO.
END.
        
{test.i "LispBoolUnknownError"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispBool({&CLNULL}, ?).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "LispBoolPredOfUnknownError"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        ret = IsCLBool(?).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "LispBoolPredOfNull"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        ret = IsCLBool({&CLNULL}).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = NO.
END.

{test.i "LispBoolPredOfWrongType"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispInteger({&CLNULL}, 4).
        ret = IsCLBool(obj).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = NO.
END.

{test.i "LispBoolPredOfBoolean"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        obj = LispBool({&CLNULL}, YES).
        ret = IsCLBool(obj).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = YES.
END.

{test.i "ABLLogicalOfUnknownError"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        ret = ABLLogical(?).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLLogicalOfNullError"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        ret = ABLLogical({&CLNULL}).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLLogicalOfWrongTypeError"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispInteger({&CLNULL}, 4).
        ret = ABLLogical(obj).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLLogicalOfTrue"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispBool({&CLNULL}, YES).
        ret = ABLLogical(obj).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = YES.
END.

{test.i "ABLLogicalOfFalse"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispBool({&CLNULL}, NO).
        ret = ABLLogical(obj).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = NO.
END.

{suiterunner.i}
