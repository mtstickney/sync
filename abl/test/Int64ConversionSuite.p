{client/include/shecl/shecl.i}
{testdata.i}

{test.i "LispInt64UnknownPoolNotNull"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR i AS INT64 NO-UNDO.

        i = {&MAXUINT32} + 10.
        obj = LispInt64(?, i).
        IF ERROR-STATUS:ERROR THEN DO:
                message "Error" view-as alert-box.
                RETURN NO.
        END.
        RETURN obj <> {&CLNULL}.
END.

{test.i "LispInt64NullPoolNotNull"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR i AS INT64 NO-UNDO.

        i = {&MAXUINT32} + 10.
        obj = LispInt64({&CLNULL}, i).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN YES.
END.

{test.i "LispInt64OfInt64Data"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR i AS INT64 NO-UNDO.
        DEFINE VAR result AS INT64 NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.

        pool = MakeObjPool().
        i = {&MAXUINT32} + 10.
        obj = LispInt64(pool, i).
        result = ABLInt64(obj).
        ReleaseObjPool(pool).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN result = i.
END.

{test.i "LispInt64OfUnknownError"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispInt64({&CLNULL}, ?).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "LispInt64PredOfNull"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        ret = IsCLInt64({&CLNULL}).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = NO.
END.

{test.i "LispInt64PredOfSubtype"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        obj = LispInteger({&CLNULL}, 4).
        ret = IsCLInt64(obj).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = YES.
END.

{test.i "LispInt64PredOfInt64"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        obj = LispInt64({&CLNULL}, {&MAXUINT32} + 10).
        ret = IsCLInt64(obj).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = YES.
END.

{test.i "LispInt64PredOfNonCoercibleType"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS LOGICAL NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.

        pool = MakeObjPool().
        obj = LispString(pool, "flub").
        ret = IsCLInt64(obj).
        ReleaseObjPool(pool).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = NO.
END.

{test.i "LispInt64PredOfUnknownError"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        ret = IsCLInt64(?).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLInt64OfUnknownError"}
        DEFINE VAR ret AS INT64 NO-UNDO.

        ret = ABLInt64(?).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLInt64OfNullError"}
        DEFINE VAR ret AS INT64 NO-UNDO.

        ret = ABLInt64({&CLNULL}).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLInt64OfSubtype"}
        DEFINE VAR ret AS INT64 NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispInteger({&CLNULL}, 4).
        ret = ABLInt64(obj).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = 4.
END.

{test.i "ABLInt64OfInt64"}
        DEFINE VAR ret AS INT64 NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispInt64({&CLNULL}, {&MAXUINT32} + 10).
        ret = ABLInt64(obj).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = {&MAXUINT32} + 10.
END.

{test.i "ABLInt64OfNonCoercibleType"}
        DEFINE VAR ret AS INT64 NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispDouble({&CLNULL}, 1.2).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        ret = ABLInt64(obj).

        RETURN ERROR-STATUS:ERROR.
END.

{suiterunner.i}