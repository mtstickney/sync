{client/include/shecl/shecl.i}
{client/include/system.i}

/* Object pool construction. */
{test.i "ObjectPoolCreation"}
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        obj = MakeObjPool() NO-ERROR.
        IF obj = 0 OR ERROR-STATUS:ERROR THEN
                RETURN NO.
        ReleaseObjPool(obj).
        RETURN YES.
END.

{test.i "ObjectPoolReleaseOfNullNoError"}
        ReleaseObjPool({&CLNULL}).
        RETURN ERROR-STATUS:ERROR = NO.
END.

{test.i "ObjectPoolReleaseOfUnknownNoError"}
        ReleaseObjPool(?).
        RETURN ERROR-STATUS:ERROR = NO.
END.

{suiterunner.i}
