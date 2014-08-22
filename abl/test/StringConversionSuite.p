{client/include/shecl/shecl.i}

{test.i "LispStringOfStringUnknownPoolNotNull"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispString(?, "Flerp").
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN obj <> {&CLNULL}.
END.

{test.i "LispStringOfStringNullPoolNotNull"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispString(0, "Flerp").
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN obj <> {&CLNULL}.
END.

{test.i "LispStringOfStringNotNull"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.

        pool = MakeObjPool().
        obj = LispString(pool, "Flerp").
        ReleaseObjPool(pool).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN obj <> {&CLNULL}.
END.

{test.i "LispStringOfUnknownError"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = LispString(?, ?).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ListStringOfStringData"}
        DEFINE VAR str AS CHARACTER NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.

        pool = MakeObjPool().
        obj = LispString(pool, "Flerp").
        str = ABLString(obj).
        ReleaseObjPool(pool).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN str = "Flerp".
END.

{test.i "LispStringPredOfString"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR pred AS LOGICAL NO-UNDO.

        pool = MakeObjPool().
        obj = LispString(pool, "Flerp").
        pred = IsCLString(obj).
        ReleaseObjPool(pool).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN pred = YES.
END.

{test.i "LispStringPredOfNull"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR pred AS LOGICAL NO-UNDO.

        pred = IsCLString({&CLNULL}).
        IF ERROR-STATUS:ERROR THEN DO:
                message "Error" view-as alert-box.
                RETURN NO.
        END.
        RETURN pred = NO.
END.

{test.i "LispStringPredOfNonCoercibleType"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR pred AS LOGICAL NO-UNDO.

        obj = LispInteger({&CLNULL}, 4).
        pred = IsCLString(obj).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN pred = NO.
END.

{test.i "LispStringPredOfUnknownError"}
        DEFINE VAR pred AS LOGICAL NO-UNDO.

        /* Trying to pass ? to a DLL procedure will generate an error anyway,
         * but that behavior is part of the contract, so test it. */
        pred = IsCLString(?).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLStringOfNull"}
        DEFINE VAR str AS CHARACTER NO-UNDO.

        str = ABLString({&CLNULL}).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLStringOfInt"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR str AS CHARACTER NO-UNDO.

        obj = LispInteger({&CLNULL}, 4).
        /* We expect an error, but this isn't a success. */
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        str = ABLSTring(obj).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLStringOfUnknownError"}
        DEFINE VAR str AS CHARACTER NO-UNDO.

        str = ABLString(?).
        RETURN ERROR-STATUS:ERROR.
END.

{test.i "ABLStringOfString"}
        DEFINE VAR str AS CHARACTER NO-UNDO.
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.

        pool = MakeObjPool().
        obj = LispString(pool, "Flub").
        str = ABLString(obj).
        ReleaseObjPool(pool).
        IF ERROR-STATUS:ERROR THEN
                RETURN NO.
        RETURN str = "Flub".
END.

{suiterunner.i}
