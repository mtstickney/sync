/* New multi-file-per-run implementation */
&SCOPED-DEFINE NANOMSG "nanomsg.dll"
&SCOPED-DEFINE NANOMSG-API EXTERNAL {&NANOMSG} CDECL PERSISTENT

/* Socket domains */
&SCOPED-DEFINE AF-SP 1
&SCOPED-DEFINE AF-SP-RAW

/* Socket types */
&SCOPED-DEFINE NN-PAIR 16
&SCOPED-DEFINE NN-REQ  48
&SCOPED-DEFINE NN-REP  49
&SCOPED-DEFINE NN-SUB  33
&SCOPED-DEFINE NN-PUB  32
&SCOPED-DEFINE NN-SURVEYOR 96
&SCOPED-DEFINE NN-RESPONDENT 97
&SCOPED-DEFINE NN-PUSH 80
&SCOPED-DEFINE NN-PULL 81
&SCOPED-DEFINE NN-BUS 112

/* Socket options */
&SCOPED-DEFINE NN-REQ-RESEND-IVL 1
&SCOPED-DEFINE NN-SUB-SUBSCRIBE 1
&SCOPED-DEFINE NN-SUB-UNSUBSCRIBE 2
&SCOPED-DEFINE NN-SURVEYOR-DEADLINE 1

&SCOPED-DEFINE NN-MSG -1
&SCOPED-DEFINE NN-DONTWAIT 1

PROCEDURE nn_socket {&NANOMSG-API}:
        DEFINE INPUT PARAMETER domain AS LONG NO-UNDO.
        DEFINE INPUT PARAMETER protocol AS LONG NO-UNDO.
        DEFINE RETURN PARAMETER fd AS LONG NO-UNDO.
END.

PROCEDURE nn_close {&NANOMSG-API}:
        DEFINE INPUT PARAMETER sock AS LONG NO-UNDO.
        DEFINE RETURN PARAMETER ret AS LONG NO-UNDO.
END.

PROCEDURE nn_bind {&NANOMSG-API}:
        DEFINE INPUT PARAMETER fd AS LONG NO-UNDO.
        DEFINE INPUT PARAMETER url AS CHARACTER NO-UNDO.
        DEFINE RETURN PARAMETER eid AS LONG NO-UNDO.
END.

PROCEDURE nn_connect {&NANOMSG-API}:
        DEFINE INPUT PARAMETER fd AS LONG NO-UNDO.
        DEFINE INPUT PARAMETER url AS CHARACTER NO-UNDO.
        DEFINE RETURN PARAMETER eid AS CHARACTER NO-UNDO.
END.

PROCEDURE nn_shutdown {&NANOMSG-API}:
        DEFINE INPUT PARAMETER fd AS LONG NO-UNDO.
        DEFINE INPUT PARAMETER eid AS LONG NO-UNDO.
        DEFINE RETURN PARAMETER ret AS LONG NO-UNDO.
END.

PROCEDURE nn_recv {&NANOMSG-API}:
        DEFINE INPUT PARAMETER fd AS LONG NO-UNDO.
        DEFINE INPUT PARAMETER buf AS LONG NO-UNDO.
        DEFINE INPUT PARAMETER bufSize AS LONG NO-UNDO.
        DEFINE INPUT PARAMETER flags AS LONG NO-UNDO.
        DEFINE RETURN PARAMETER bytes AS LONG NO-UNDO.
END.

PROCEDURE nn_send {&NANOMSG-API}:
        DEFINE INPUT PARAMETER fd AS LONG NO-UNDO.
        DEFINE INPUT PARAMETER buf AS LONG NO-UNDO.
        DEFINE INPUT PARAMETER bufSize AS LONG NO-UNDO.
        DEFINE INPUT PARAMETER flags AS LONG NO-UNDO.
        DEFINE RETURN PARAMETER bytes AS LONG NO-UNDO.
END.

PROCEDURE nn_allocmsg {&NANOMSG-API}:
        DEFINE INPUT PARAMETER size AS LONG NO-UNDO.
        DEFINE INPUT PARAMETER type AS LONG NO-UNDO.
        DEFINE RETURN PARAMETER ptr AS LONG NO-UNDO.
END.

PROCEDURE nn_freemsg {&NANOMSG-API}:
        DEFINE INPUT PARAMETER ptr AS LONG NO-UNDO.
        DEFINE RETURN PARAMETER ret AS LONG NO-UNDO.
END.

PROCEDURE nn_errno {&NANOMSG-API}:
        DEFINE RETURN PARAMETER errno AS LONG NO-UNDO.
END.

PROCEDURE nn_strerror {&NANOMSG-API}:
        DEFINE INPUT PARAMETER errno AS LONG NO-UNDO.
        DEFINE RETURN PARAMETER msg AS CHARACTER NO-UNDO.
END.

PROCEDURE Sleep EXTERNAL "kernel32.dll" STDCALL:
        DEFINE INPUT PARAMETER millis AS LONG NO-UNDO.
END.

/* Higher-level wrappers for message passing. */
PROCEDURE SendMsg:
        DEFINE INPUT PARAMETER sock AS INTEGER NO-UNDO.
        DEFINE INPUT PARAMETER msg AS CHARACTER NO-UNDO.

        DEFINE VAR ptrptr AS INTEGER NO-UNDO.
        DEFINE VAR bufptr AS INTEGER NO-UNDO.
        DEFINE VAR bufptrbuf AS MEMPTR NO-UNDO.
        DEFINE VAR buf AS MEMPTR NO-UNDO.
        DEFINE VAR bytes AS INTEGER NO-UNDO.
        DEFINE VAR errno AS INTEGER NO-UNDO.
        DEFINE VAR errmsg AS CHARACTER NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

ReportError:
DO ON ERROR UNDO, LEAVE:
        RUN nn_allocmsg(LENGTH(msg), 0, OUTPUT bufptr).
        IF bufptr = 0 THEN
                LEAVE ReportError.

FreeMsg:
DO ON ERROR UNDO, LEAVE:
        SET-POINTER-VALUE(buf) = bufptr.
        PUT-STRING(buf, 1, LENGTH(msg)) = msg.

        /* If you don't set-size = 0 first, memptrs get reused in a weird way
         * (try returning one from a function sometime). */
        SET-SIZE(bufptrbuf) = 0.
        SET-SIZE(bufptrbuf) = 4.

FreeBuf:
DO ON ERROR UNDO, LEAVE:
        PUT-LONG(bufptrbuf, 1) = bufptr.
        ptrptr = GET-POINTER-VALUE(bufptrbuf).
        RUN nn_send(sock, ptrptr, {&NN-MSG}, 0, OUTPUT bytes).
        IF bytes < 0 THEN
                LEAVE FreeBuf.

        SET-SIZE(bufptrbuf) = 0.
        RETURN.

END. /* LABEL FreeBuf */
        SET-SIZE(bufptrbuf) = 0.

END. /* LABEL FreeMsg */
        /* If bytes >= 0, the send succeeded and the message was freed by nn_send(). */
        IF NOT bytes >= 0 THEN
                RUN nn_freemsg(bufptr, OUTPUT ret).

END. /* LABEL ReportError */
        RUN nn_errno(OUTPUT errno).
        RUN nn_strerror(errno, OUTPUT errmsg).
        RETURN ERROR errmsg.
END.


FUNCTION compileFile RETURNS LOGICAL (INPUT srcFile AS CHARACTER, INPUT outDir AS CHARACTER, OUTPUT msgs AS CHARACTER):
        DEFINE VAR i AS INTEGER NO-UNDO.
        COMPILE VALUE(srcFile)
        SAVE INTO VALUE(outDir)
        GENERATE-MD5 NO-ERROR.

        msgs = "".
        IF COMPILER:ERROR THEN DO:
                DO i = 1 TO COMPILER:NUM-MESSAGES:
                        msgs = msgs + COMPILER:GET-MESSAGE(i) + "~n".
                END.
        END.

        RETURN YES.
END.

PROCEDURE RecvMsg:
        DEFINE INPUT PARAMETER sock AS INTEGER NO-UNDO.
        DEFINE OUTPUT PARAMETER msg AS CHARACTER NO-UNDO.

        DEFINE VAR ptrptr AS INTEGER NO-UNDO.
        DEFINE VAR bufptr AS INTEGER NO-UNDO.
        DEFINE VAR bufptrbuf AS MEMPTR NO-UNDO.
        DEFINE VAR buf AS MEMPTR NO-UNDO.
        DEFINE VAR bytes AS INTEGER NO-UNDO.
        DEFINE VAR errno AS INTEGER NO-UNDO.
        DEFINE VAR errmsg AS CHARACTER NO-UNDO.
        DEFINE VAR ret AS INTEGER NO-UNDO.

        SET-SIZE(bufptrbuf) = 0.
        SET-SIZE(bufptrbuf) = 4.

FreeBuf:
DO ON ERROR UNDO, LEAVE:
        PUT-LONG(bufptrbuf, 1) = 0.
        ptrptr = GET-POINTER-VALUE(bufptrbuf).
        RUN nn_recv(sock, ptrptr, {&NN-MSG}, 0, OUTPUT bytes).
        IF bytes < 0 THEN
                LEAVE FreeBuf.

        bufptr = GET-LONG(bufptrbuf, 1).
        SET-POINTER-VALUE(buf) = bufptr.
        msg = GET-STRING(buf, 1, bytes).

        SET-SIZE(bufptrbuf) = 0.
        RETURN.

END. /* LABEL FreeBuf */
        SET-SIZE(bufptrbuf) = 0.
        RUN nn_errno(OUTPUT errno).
        RUN nn_strerror(errno, OUTPUT errmsg).
        RETURN ERROR errmsg.
END.

DEFINE var line AS CHARACTER NO-UNDO.
DEFINE VAR file AS CHARACTER NO-UNDO.
DEFINE VAR outputDir AS CHARACTER NO-UNDO.
DEFINE VAR sourceDir AS CHARACTER NO-UNDO.
DEFINE VAR compilerOutput AS CHARACTER NO-UNDO.

DEFINE VAR sock AS INTEGER NO-UNDO.
DEFINE VAR eid AS INTEGER NO-UNDO.
DEFINE VAR errno AS INTEGER NO-UNDO.
DEFINE VAR errmsg AS CHARACTER NO-UNDO.
DEFINE VAR url AS CHARACTER NO-UNDO.
DEFINE VAR ret AS INTEGER NO-UNDO.

DEFINE VAR origPropath AS CHARACTER NO-UNDO.

origPropath = PROPATH.
url = SESSION:PARAMETER.

/* Connect the message socket. */
ReportError:
DO ON ERROR UNDO, LEAVE:
        RUN nn_socket({&AF-SP}, {&NN-PAIR}, OUTPUT sock).
        IF sock < 0 THEN
                LEAVE ReportError.

CloseSocket:
DO ON ERROR UNDO, LEAVE:
        RUN nn_bind(sock, url, OUTPUT eid).
        IF eid < 0 THEN
                LEAVE CloseSocket.

ShutdownEndpoint:
DO ON ERROR UNDO, LEAVE:
        BuildLoop:
        REPEAT ON ERROR UNDO, LEAVE ShutdownEndpoint:
                RUN RecvMsg(sock, OUTPUT line).
                /* An empty message indicates the end of the build list. */
                IF line = "":U THEN
                        LEAVE BuildLoop.

                sourceDir = ENTRY(1, line).
                file = ENTRY(2, line).
                outputDir = ENTRY(3, line).

                PROPATH = sourceDir + "," + origPropath.
                IF compileFile(file, outputDir, OUTPUT compilerOutput) <> YES THEN
                        /* Bail so that the build tool can pick up the error. */
                        LEAVE BuildLoop.
                /* Output a newline so that we know the build is done. */
                RUN SendMsg(sock, compilerOutput).
        END.

        RUN Sleep(100).
        RUN nn_shutdown(sock, eid, OUTPUT ret).
        RUN nn_close(sock, OUTPUT ret).
        QUIT.

END. /* LABEL ShutdownEndpoint */
        RUN Sleep(100).
        RUN nn_shutdown(sock, eid, OUTPUT ret).

END. /* LABEL CloseSocket */
        RUN Sleep(100).
        RUN nn_close(sock, OUTPUT ret).

END. /* LABEL ReportError */
        RUN nn_errno(OUTPUT errno).
        RUN nn_strerror(errno, OUTPUT errmsg).
        RETURN ERROR errmsg.

QUIT.
