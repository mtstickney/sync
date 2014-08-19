&IF defined(SHEC_I_)=0 &THEN
&GLOBAL-DEFINE SHECL_I_

{client/include/shecl/shecl_ffi.i}
{client/include/system.i}
{client/include/ffi.i}
{client/include/errors.i}

FUNCTION SheclInit RETURNS LOGICAL():
        DEFINE VAR faslPath AS CHARACTER NO-UNDO.
        DEFINE VAR argv1 AS MEMPTR NO-UNDO.
        DEFINE VAR argv AS MEMPTR NO-UNDO.
        DEFINE VAR pointerBytes AS INTEGER INITIAL 8 NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        faslPath = FindFile("client/bin/shecl.fasb").
        argv1 = ForeignString("ecl.dll").
        SET-SIZE(argv) = pointerBytes.
        PUT-INT64(argv, 1) = GET-POINTER-VALUE(argv1).
        RUN shecl_boot(faslPath, 1, argv, OUTPUT ret).

        IF ret < 0 THEN DO:
                Errors:ThrowError("Error initializing Shecl system.").
                RETURN NO.
        END.
        RETURN YES.
END.

FUNCTION SheclShutdown RETURNS LOGICAL():
        RUN shecl_shutdown.
        RETURN YES.
END.

FUNCTION Eval RETURNS {&CLOBJECT} (INPUT pool AS {&CLOBJECT}, INPUT str AS CHARACTER):
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        IF pool = ? THEN
                /* Pass NULL for no pool. */
                pool = 0.

        RUN eval(str, pool, OUTPUT obj).
        RUN CheckForErrors.
        RETURN obj.
END.

FUNCTION Read RETURNS {&CLOBJECT} (INPUT pool AS {&CLOBJECT}, INPUT str AS CHARACTER):
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        IF pool = ? THEN
                /* Pass NULL for no pool. */
                pool = 0.

        RUN read(str, pool, OUTPUT obj).
        RUN CheckForErrors.
        RETURN obj.
END.

FUNCTION Nil RETURNS {&CLOBJECT} ():
        RETURN Read("nil").
END.

FUNCTION MakeObjPool RETURNS {&CLOBJECT} ():
        DEFINE VAR makePool AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR pool AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR nil AS {&CLOBJECT} NO-UNDO.

        /* Eval("(shecl:make-pool)") */
        /* Note: it's important to set nil before reading
         * the make-pool symbol, so the garbage collector can't
         * reap the symbol before the funcall happens (NIL will
         * never be collected, so it's safe). */
        nil = Nil().
        makePool = Read("shecl:make-pool").
        RETURN Call1(makePool, nil).
END.

FUNCTION ReleaseObjPool RETURNS LOGICAL (INPUT pool AS {&CLOBJECT}):
        DEFINE VAR releasePool AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR nil AS {&CLOBJECT NO-UNDO.

        /* (shecl:release-pool pool) */
        /* Note: it's important to set nil first here, see note in
         * MakeObjPool(). */
        nil = Nil().
        releasePool = Read("shecl:release-pool").
        RETURN Call2(releasePool, pool, nil).
END.

/* FIXME: type conversion stuff isn't using object pools. */
FUNCTION LispString RETURNS {&CLOBJECT} (INPUT pool AS {&CLOBJECT}, INPUT str AS CHARACTER):
        DEFINE VAR ret AS {&CLOBJECT} NO-UNDO.

        RUN lisp_string(pool, str, OUTPUT ret).
        RUN CheckForErrors.
        RETURN ret.
END.

FUNCTION IsCLString RETURNS LOGICAL (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR ret AS INTEGER NO-UNDO.

        RUN string_p(obj, OUTPUT ret).
        IF ret < -1 THEN
                Errors:ThrowError("Error while checking the string-ness of an object.").
        ELSE IF ret = 0 THEN
                RETURN NO.
        ELSE
                RETURN YES.
END.

FUNCTION ABLString RETURNS CHARACTER (INPUT obj AS {&CLOBJECT}
        DEFINE VAR ret AS MEMPTR NO-UNDO.
        DEFINE VAR str AS CHARACTER NO-UNDO.

        IF NOT IsCLString(obj) THEN
                Errors:Error("obj is not a lisp string, can't convert it to an ABL string.").
        RUN c_string(obj, OUTPUT ret).
        IF GET-POINTER-VALUE(ret) = 0 THEN
                Errors:ThrowError("Error converting object to C string.").
        str = GET-STRING(ret, 1).
        SET-SIZE(ret) = 0.
        RETURN str.
END.

FUNCTION LispDecimal RETURNS {&CLOBJECT} (INPUT pool AS {&CLOBJECT}, INPUT d AS DECIMAL):
        DEFINE VAR ret AS DECIMAL NO-UNDO.

        RUN lisp_double(pool, d, OUTPUT ret).
        RUN CheckForErrors.
        RETURN ret.
END.

FUNCTION IsCLDouble RETURNS LOGICAL (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR ret AS INTEGER NO-UNDO.

        RUN double_p(obj, OUTPUT ret).
        IF ret < 0 THEN
                Errors:ThrowError("Error while checking the double-ness of an object.").
        ELSE IF ret = 0 THEN
                RETURN NO.
        ELSE
                RETURN YES.
END.

FUNCTION ABLDecimal RETURNS DECIMAL (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR ret AS INTEGER NO-UNDO.
        DEFINE VAR d AS DECIMAL NO-UNDO.

        IF NOT IsCLDouble(obj) THEN
                Errors:Error("obj is not a double, can't convert it to a DECIMAL.").
        RUN c_double(obj, OUTPUT d, OUTPUT ret).
        IF ret <> 0 THEN
                Errors:ThrowError("Error converting object to C double.").
        RETURN d.
END.

FUNCTION LispInt64 RETURNS {&CLOBJECT} (INPUT pool AS {&CLOBJECT}, INPUT i AS INT64):
        DEFINE VAR ret AS {&CLOBJECT} NO-UNDO.

        RUN lisp_int64(pool, i, OUTPUT ret).
        RUN CheckForErrors.
        RETURN ret.
END.

FUNCTION IsCLInt64 RETURNS LOGICAL (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR ret AS INTEGER NO-UNDO.

        RUN int64_p(obj, OUTPUT ret).
        IF ret < 0 THEN
                Errors:ThrowError("Error checking the int64-ness of an object.").
        ELSE IF ret = 0 THEN
                RETURN NO.
        ELSE
                RETURN YES.
END.

FUNCTION ABLInt64 RETURNS INT64 (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR i AS INT64 NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        IF NOT isCLInt64(obj) THEN
                Errors:ThrowError("obj is not a 64-bit int, can't convert it to INT64.").
        RUN c_int64(obj, OUTPUT i, OUTPUT ret).
        IF ret <> 0 THEN
                Errors:ThrowError("Error converting object to 64-bit int.").
        RETURN i.
END.

FUNCTION LispBool RETURNS {&CLOBJECT} (INPUT pool AS {&CLOBJECT}, INPUT b AS LOGICAL):
        DEFINE VAR ret AS {&CLOBJECT} NO-UNDO.

        IF b = ? THEN
                Errors:ThrowError("Cannot convert the unknown value to a boolean.").
        IF b THEN
                RUN lisp_bool(pool, 1, OUTPUT ret).
        ELSE
                RUN lisp_bool(pool, 0, OUTPUT ret).
        RUN CheckForErrors.
        RETURN ret.
END.

FUNCTION IsCLBool RETURNS LOGICAL (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR ret AS INTEGER NO-UNDO.

        RUN bool_p(obj, OUTPUT ret).
        IF ret < 0 THEN
                Errors:ThrowError("Error checking boolean-ness of object.").
        ELSE IF ret = 0 THEN
                RETURN NO.
        ELSE
                RETURN YES.
END.

FUNCTION ABLLogical RETURNS LOGICAL (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR b AS INTEGER NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        IF NOT IsCLBool(obj) THEN
                Errors:ThrowError("Can't convert non-boolean object to LOGICAL.").
        RUN c_bool(obj, OUTPUT b, OUTPUT ret).
        IF ret <> 0 THEN
                Errors:ThrowError("Error converting object to LOGICAL.").

        IF b = 0 THEN
                RETURN NO.
        ELSE
                RETURN YES.
END.

FUNCTION GeneralizedABLLogical RETURNS LOGICAL (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR b AS INTEGER NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        RUN c_generalized_bool(obj, OUTPUT b, OUTPUT ret).
        IF ret <> 0 THEN
                Errors:ThrowError("Error converting object to LOGICAL.").

        IF b = 0 THEN
                RETURN NO.
        ELSE
                RETURN YES.
END.

&ENDIF
