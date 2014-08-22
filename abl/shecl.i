&IF defined(SHECL_I_)=0 &THEN
&GLOBAL-DEFINE SHECL_I_

{client/include/shecl/shecl_ffi.i}
{client/include/ffi.i}
{client/include/errors.i}
{client/include/shecl/call.i 2}

&GLOBAL-DEFINE CLNULL 0

FUNCTION SheclInit RETURNS LOGICAL():
        DEFINE VAR faslPath AS CHARACTER NO-UNDO.
        DEFINE VAR numArgs AS INTEGER INITIAL 1 NO-UNDO.
        DEFINE VAR argvs AS MEMPTR EXTENT NO-UNDO.
        DEFINE VAR argv AS MEMPTR NO-UNDO.
        DEFINE VAR argvPtr AS {&ABL_POINTER} NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.
        DEFINE VAR i AS INTEGER NO-UNDO.

        faslPath = SEARCH("client/bin/shecl-bootstrap.fasb").

        EXTENT(argvs) = numArgs.
        argvs[1] = ForeignString("ecl.dll").

        SET-SIZE(argv) = {&POINTER_BYTES} * numArgs.

        DO i = 1 TO numArgs:
/* Note that we're not using PROCESS-ARCHITECTURE here, as it may not be available. */
&IF {&POINTER_BYTES} = 8 &THEN
                PUT-INT64(argv, i) = GET-POINTER-VALUE(argvs[i]).
                message "put int64 pointer in argv" view-as alert-box.
&ELSE
                PUT-LONG(argv, i) = GET-POINTER-VALUE(argvs[i]).
                message "put long pointer in argv" view-as alert-box.
&ENDIF
        END.
        argvPtr = GET-POINTER-VALUE(argv).
        RUN shecl_boot IN sheclApi (faslPath, numArgs, argvPtr, OUTPUT ret).

        DO i = 1 TO numArgs:
                SET-SIZE(argvs[i]) = 0.
        END.
        SET-SIZE(argv) = 0.

        IF ret < 0 THEN DO:
                Errors:Error("Error initializing Shecl system.") NO-ERROR.
                RETURN NO.
        END.
        RETURN YES.
END.

FUNCTION SheclShutdown RETURNS LOGICAL():
        RUN shecl_shutdown IN sheclApi.
        RETURN YES.
END.

FUNCTION Eval RETURNS {&CLOBJECT} (INPUT pool AS {&CLOBJECT}, INPUT str AS CHARACTER):
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.

        IF pool = ? THEN
                /* Pass NULL for no pool. */
                pool = {&CLNULL}.

        RUN eval IN sheclApi (str, pool, OUTPUT obj).
        RUN CheckForErrors IN sheclApi NO-ERROR.
        RETURN obj.
END.

FUNCTION Read RETURNS {&CLOBJECT} (INPUT pool AS {&CLOBJECT}, INPUT str AS CHARACTER):
        DEFINE VAR obj AS {&CLOBJECT} NO-UNDO.
        IF pool = ? THEN
                /* Pass NULL for no pool. */
                pool = {&CLNULL}.

        RUN read IN sheclApi (str, pool, OUTPUT obj).
        RUN CheckForErrors IN sheclApi NO-ERROR.
        RETURN obj.
END.

/* TODO: Add header for multi-value apply calls. */
{client/include/shecl/call.i 1}

FUNCTION Nil RETURNS {&CLOBJECT} ():
        RETURN Read(0, "nil").
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
        makePool = Read(0, "shecl:make-pool").
        /* Pool is saved when created, no need to create it in a pool of its own. */
        RETURN Call1(0, makePool, nil).
END.

FUNCTION ReleaseObjPool RETURNS LOGICAL (INPUT pool AS {&CLOBJECT}):
        DEFINE VAR releasePool AS {&CLOBJECT} NO-UNDO.
        DEFINE VAR nil AS {&CLOBJECT} NO-UNDO.

        /* (shecl:release-pool pool) */
        /* Note: it's important to set nil first here, see note in
         * MakeObjPool(). */
        nil = Nil().
        releasePool = Read(0, "shecl:release-pool").
        /* No return value, don't bother with a pool. */
        Call2(0, releasePool, pool, nil).
        RETURN YES.
END.

/* FIXME: type conversion stuff isn't using object pools. */
FUNCTION LispString RETURNS {&CLOBJECT} (INPUT pool AS {&CLOBJECT}, INPUT str AS CHARACTER):
        DEFINE VAR ret AS {&CLOBJECT} NO-UNDO.

        IF pool = ? THEN
                pool = {&CLNULL}.

        IF str = ? THEN DO:
                Errors:Error("Unknown is not a valid character, cannot convert it to a lisp object.") NO-ERROR.
                RETURN ?.
        END.
        RUN lisp_string IN sheclApi (pool, str, OUTPUT ret).
        RUN CheckForErrors IN sheclApi NO-ERROR.
        RETURN ret.
END.

FUNCTION IsCLString RETURNS LOGICAL (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR ret AS INTEGER NO-UNDO.

        IF obj = ? THEN DO:
                Errors:Error("Unknown is not a valid lisp object, cannot check its type.") NO-ERROR.
                RETURN ?.
        END.
        RUN string_p IN sheclApi (obj, OUTPUT ret).
        IF ret < -1 THEN DO:
                Errors:Error("Error while checking the string-ness of an object.") NO-ERROR.
                RETURN ?.
        END.
        ELSE IF ret = 0 THEN
                RETURN NO.
        ELSE
                RETURN YES.
END.

FUNCTION ABLString RETURNS CHARACTER (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR ret AS MEMPTR NO-UNDO.
        DEFINE VAR str AS CHARACTER NO-UNDO.

        IF obj = ? THEN DO:
                Errors:Error("Unknown is not a valid lisp object, cannot convert it to character.") NO-ERROR.
                RETURN ?.
        END.
        IF NOT IsCLString(obj) THEN DO:
                Errors:Error("obj is not a lisp string, can't convert it to an ABL string.") NO-ERROR.
                RETURN ?.
        END.
        RUN c_string IN sheclApi (obj, OUTPUT ret).
        IF GET-POINTER-VALUE(ret) = 0 THEN DO:
                Errors:Error("Error converting object to C string.") NO-ERROR.
                RETURN ?.
        END.
        str = GET-STRING(ret, 1).
        SET-SIZE(ret) = 0.
        RETURN str.
END.

FUNCTION LispDouble RETURNS {&CLOBJECT} (INPUT pool AS {&CLOBJECT}, INPUT d AS DECIMAL):
        DEFINE VAR ret AS {&CLOBJECT} NO-UNDO.

        IF pool = ? THEN
                pool = {&CLNULL}.

        IF d = ? THEN DO:
                Errors:Error("Unknown is not a valid decimal, cannot convert it to a lisp object.") NO-ERROR.
                RETURN ?.
        END.
        RUN lisp_double IN sheclApi (pool, d, OUTPUT ret).
        RUN CheckForErrors IN sheclApi NO-ERROR.
        RETURN ret.
END.

FUNCTION IsCLDouble RETURNS LOGICAL (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR ret AS INTEGER NO-UNDO.

        IF obj = ? THEN DO:
                Errors:Error("Unknown is not a valid lisp object, cannot check its type.") NO-ERROR.
                RETURN ?.
        END.
        RUN double_p IN sheclApi(obj, OUTPUT ret).
        IF ret < 0 THEN DO:
                Errors:Error("Error while checking the double-ness of an object.") NO-ERROR.
                RETURN ?.
        END.
        ELSE IF ret = 0 THEN
                RETURN NO.
        ELSE
                RETURN YES.
END.

FUNCTION ABLDecimal RETURNS DECIMAL (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR ret AS INTEGER NO-UNDO.
        DEFINE VAR d AS DECIMAL NO-UNDO.

        IF obj = ? THEN DO:
                Errors:Error("Unknown is not a valid lisp object, cannot convert it to decimal.") NO-ERROR.
                RETURN ?.
        END.
        IF NOT IsCLDouble(obj) THEN DO:
                Errors:Error("obj is not a double, can't convert it to a DECIMAL.") NO-ERROR.
                RETURN ?.
        END.
        RUN c_double IN sheclApi (obj, OUTPUT d, OUTPUT ret).
        IF ret <> 0 THEN DO:
                Errors:Error("Error converting object to C double.") NO-ERROR.
                RETURN ?.
        END.
        RETURN d.
END.

FUNCTION LispInt64 RETURNS {&CLOBJECT} (INPUT pool AS {&CLOBJECT}, INPUT i AS INT64):
        DEFINE VAR ret AS {&CLOBJECT} NO-UNDO.

        IF pool = ? THEN
                pool = {&CLNULL}.

        IF i = ? THEN DO:
                Errors:Error("Unknown is not a valid int64, cannot convert it to a lisp object.") NO-ERROR.
                RETURN ?.
        END.
        RUN lisp_int64 IN sheclApi (pool, i, OUTPUT ret).
        RUN CheckForErrors IN sheclApi NO-ERROR.
        RETURN ret.
END.

FUNCTION IsCLInt64 RETURNS LOGICAL (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR ret AS INTEGER NO-UNDO.

        IF obj = ? THEN DO:
                Errors:Error("Unknown is not a valid lisp object, cannot check its type.") NO-ERROR.
                RETURN ?.
        END.
        RUN int64_p IN sheclApi (obj, OUTPUT ret).
        IF ret < 0 THEN DO:
                Errors:Error("Error checking the int64-ness of an object.") NO-ERROR.
                RETURN ?.
        END.
        ELSE IF ret = 0 THEN
                RETURN NO.
        ELSE
                RETURN YES.
END.

FUNCTION ABLInt64 RETURNS INT64 (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR i AS INT64 NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        IF obj = ? THEN DO:
                Errors:Error("Unknown is not a valid lisp object, and cannot be converted to int64.") NO-ERROR.
                RETURN ?.
        END.
        IF NOT IsCLInt64(obj) THEN DO:
                Errors:Error("obj is not a 64-bit int, can't convert it to INT64.") NO-ERROR.
                RETURN ?.
        END.
        RUN c_int64 IN sheclApi (obj, OUTPUT i, OUTPUT ret).
        IF ret <> 0 THEN DO:
                Errors:Error("Error converting object to 64-bit int.") NO-ERROR.
                RETURN ?.
        END.
        RETURN i.
END.

FUNCTION LispInteger RETURNS {&CLOBJECT} (INPUT pool AS {&CLOBJECT}, INPUT i AS INTEGER):
        DEFINE VAR ret AS {&CLOBJECT} NO-UNDO.

        IF pool = ? THEN
                pool = {&CLNULL}.

        IF i = ? THEN DO:
                Errors:Error("Unknown is not a valid integer, cannot convert it to a lisp int.") NO-ERROR.
                RETURN ?.
        END.
        RUN lisp_int IN sheclApi (pool, i, OUTPUT ret).
        RUN CheckForErrors IN sheclApi NO-ERROR.
        RETURN ret.
END.

FUNCTION IsCLInteger RETURNS LOGICAL (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR ret AS INTEGER NO-UNDO.

        IF obj = ? THEN DO:
                Errors:Error("Unknown is not a valid lisp object, cannot check its type.") NO-ERROR.
                RETURN ?.
        END.
        RUN int_p IN sheclApi (obj, OUTPUT ret).
        IF ret < 0 THEN DO:
                Errors:Error("Error checking the int32-ness of an object.") NO-ERROR.
                RETURN ?.
        END.
        ELSE IF ret = 0 THEN
                RETURN NO.
        ELSE
                RETURN YES.
END.

FUNCTION ABLInteger RETURNS INTEGER (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR i AS INTEGER NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        IF obj = ? THEN DO:
                Errors:Error("Unknown is not a valid lisp object, cannot convert it to an integer.") NO-ERROR.
                RETURN ?.
        END.
        IF NOT IsCLInteger(obj) THEN DO:
                Errors:Error("obj is not a 32-bit int, can't convert it to INTEGER.") NO-ERROR.
                RETURN ?.
        END.
        RUN c_int IN sheclApi (obj, OUTPUT i, OUTPUT ret).
        IF ret <> 0 THEN DO:
                Errors:Error("Error converting object to 32-bit int.") NO-ERROR.
                RETURN ?.
        END.
        RETURN i.
END.

FUNCTION LispBool RETURNS {&CLOBJECT} (INPUT pool AS {&CLOBJECT}, INPUT b AS LOGICAL):
        DEFINE VAR ret AS {&CLOBJECT} NO-UNDO.

        IF pool = ? THEN
                pool = {&CLNULL}.

        IF b = ? THEN DO:
                Errors:Error("Cannot convert the unknown value to a boolean.") NO-ERROR.
                RETURN ?.
        END.
        IF b THEN
                RUN lisp_bool IN sheclApi (pool, 1, OUTPUT ret).
        ELSE
                RUN lisp_bool IN sheclApi (pool, 0, OUTPUT ret).
        RUN CheckForErrors IN sheclApi NO-ERROR.
        RETURN ret.
END.

FUNCTION IsCLBool RETURNS LOGICAL (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR ret AS INTEGER NO-UNDO.

        IF obj = ? THEN DO:
                Errors:Error("Unknown is not a valid lisp object, cannot check its type.") NO-ERROR.
                RETURN ?.
        END.
        RUN bool_p IN sheclApi (obj, OUTPUT ret).
        IF ret < 0 THEN DO:
                Errors:Error("Error checking boolean-ness of object.") NO-ERROR.
                RETURN ?.
        END.
        ELSE IF ret = 0 THEN
                RETURN NO.
        ELSE
                RETURN YES.
END.

FUNCTION ABLLogical RETURNS LOGICAL (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR b AS INTEGER NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        IF obj = ? THEN DO:
                Errors:Error("Unknown is not a valid lisp object, cannot convert to a logical.") NO-ERROR.
                RETURN ?.
        END.
        IF NOT IsCLBool(obj) THEN DO:
                Errors:Error("Can't convert non-boolean object to LOGICAL.") NO-ERROR.
                RETURN ?.
        END.
        RUN c_bool IN sheclApi (obj, OUTPUT b, OUTPUT ret).
        IF ret <> 0 THEN DO:
                Errors:Error("Error converting object to LOGICAL.") NO-ERROR.
                RETURN ?.
        END.

        IF b = 0 THEN
                RETURN NO.
        ELSE
                RETURN YES.
END.

FUNCTION ABLGeneralizedLogical RETURNS LOGICAL (INPUT obj AS {&CLOBJECT}):
        DEFINE VAR b AS INTEGER NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        IF obj = ? THEN DO:
                Errors:Error("Uknown is not a valid lisp object, cannot convert to a logical.") NO-ERROR.
                RETURN ?.
        END.
        IF obj = {&CLNULL} THEN DO:
                Errors:Error("The NULL object is not a valid lisp object, cannot convert to a logical.") NO-ERROR.
                RETURN ?.
        END.
        RUN c_generalized_bool IN sheclApi (obj, OUTPUT b, OUTPUT ret).
        IF ret <> 0 THEN DO:
                Errors:Error("Error converting object to LOGICAL.") NO-ERROR.
                RETURN ?.
        END.

        IF b = 0 THEN
                RETURN NO.
        ELSE
                RETURN YES.
END.

&ENDIF
