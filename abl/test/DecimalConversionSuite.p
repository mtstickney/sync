{client/include/shecl/shecl.i}

{test.i "LispDoubleOfDecimalUnknownPoolNotNull"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispDouble(?, 3.1).
        IF ERROR-STATUS:ERROR THEN DO:
                Message "There vas errorz." view-as alert-box.
                Message "Error vas: " ERROR-STATUS:GET-MESSAGE(1) view-as alert-box.
                RETURN NO.
        END.
        RETURN obj <> {&CLNULL}.
END.

{test.i "LispDoubleOfDecimalNullPoolNotNull"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispDouble({&CLNULL}, 3.1).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN obj <> {&CLNULL}.
END.

{test.i "LispDoubleOfUnknownError"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispDouble(0, ?).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "LispDoubleOfDecimalData"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR num AS DECIMAL NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.

        pool = MakeObjPool().
        obj = LispDouble(pool, 3.1).
        num = ABLDecimal(obj).
        ReleaseObjPool(pool).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN num = 3.1.
END.

{test.i "LispDoublePredOfNull"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        ret = IsCLDouble({&CLNULL}).

        IF ERROR-STATUS:ERROR THEN DO:
                MESSAGE "Error: " error-status:get-message(1) view-as alert-box.
                RETURN NO.
        END.
        RETURN ret = NO.
END.

{test.i "LispDoublePredOfNonCoercibleType"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        pool = MakeObjPool().
        obj = LispString(pool, "flub").
        ret = IsCLDouble(obj).
        ReleaseObjPool(pool).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = NO.
END.

{test.i "LispDoublePredOfCoercibleType"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        /* INTEGER is not a subtype of DOUBLE-FLOAT, so we should get a NIL here. */
        pool = MakeObjPool().
        obj = LispInteger({&CLNULL}, 4).
        ret = IsCLDouble(obj).
        ReleaseObjPool(pool).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = NO.
END.

{test.i "LispDoublePredOfDouble"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        pool = MakeObjPool().
        obj = Read(pool, "1.2d0"). /* Read a double-float */
        ret = IsCLDouble(obj).
        ReleaseObjPool(pool).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = YES.
END.

{test.i "LispDoublePredOfUnknownError"}
        DEFINE VAR ret AS LOGICAL NO-UNDO.

        ret = IsCLDouble(?).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLDecimalOfNullError"}
        DEFINE VAR ret AS DECIMAL NO-UNDO.

        ret = ABLDecimal({&CLNULL}).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLDecimalOfNonDoubleError"}
        DEFINE VAR ret AS DECIMAL NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR hadError AS LOGICAL NO-UNDO.

        obj = LispInteger({&CLNULL}, 4).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        ret = ABLDecimal(obj).
        hadError = ERROR-STATUS:ERROR.
        RETURN hadError.
END.

{test.i "ABLDecimalOfDecimal"}
        DEFINE VAR ret AS DECIMAL NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.

        pool = MakeObjPool().
        obj = LispDouble(pool, 1.3).
        ret = ABLDecimal(obj).
        ReleaseObjPool(pool).

        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN ret = 1.3.
END.

{test.i "ABLDecimalOfUnknownError"}
        DEFINE VAR ret AS DECIMAL NO-UNDO.

        ret = ABLDecimal(?).
        RETURN ERROR-STATUS:ERROR.
END.

{suiterunner.i}
