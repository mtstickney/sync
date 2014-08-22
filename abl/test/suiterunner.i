&IF defined(SUITE_RUNNER_I_)=0 &THEN
&GLOBAL-DEFINE SUITE_RUNNER_I_

{client/include/system.i}

DEFINE VAR passed AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VAR failed AS INTEGER INITIAL 0 NO-UNDO.

FUNCTION Pass RETURNS LOGICAL ():
        passed = passed + 1.
        RETURN YES.
END.

FUNCTION Fail RETURNS LOGICAL ():
        failed = failed + 1.
        RETURN YES.
END.

PROCEDURE RunSuite:
        DEFINE INPUT PARAMETER name AS CHARACTER NO-UNDO.
        DEFINE INPUT PARAMETER binDir AS CHARACTER NO-UNDO.
        DEFINE OUTPUT PARAMETER pass AS LOGICAL NO-UNDO.

        DEFINE VAR tests AS CHARACTER EXTENT INITIAL [{&TEST_LIST}] NO-UNDO.
        DEFINE VAR i AS INTEGER NO-UNDO.
        DEFINE VAR result AS LOGICAL NO-UNDO.
        DEFINE VAR thisTest AS CHARACTER NO-UNDO.
        DEFINE VAR path AS CHARACTER NO-UNDO.

        /* Now run the actual tests. */
        pass = YES.
        DO i = 1 TO EXTENT(tests):
                thisTest = tests[i].
                result = DYNAMIC-FUNCTION(tests[i]) NO-ERROR.
                IF ERROR-STATUS:ERROR OR result <> YES THEN DO:
                        Fail().
                        PUT UNFORMATTED SKIP "[FAIL]  " thisTest.
                        pass = NO.
                END.
                ELSE IF result THEN DO:
                        Pass().
                        PUT UNFORMATTED SKIP "[PASS]  " thisTest.
                END.
        END.

        PUT UNFORMATTED SKIP "-----".
        PUT UNFORMATTED SKIP name ": " {&TEST_COUNT} " tests, " passed " passed, " failed " failed." SKIP(1).
END.

&ENDIF
