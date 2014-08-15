#include "shecl.h"

/* At one time this function did something useful. It might again someday, so leave it here. */
int shecl_boot(int argc, char **argv)
{
        /* TODO: load the lisp-side shecl system here (or use a system dll or something). */
        return cl_boot(argc, argv);
}

void shecl_shutdown(void)
{
        cl_shutdown();
}

/* TODO: add out-of-band error parameter (cl_object* or something). */
cl_object eval(const char *s, cl_object pool)
{
        cl_object form;
        cl_object val;
        cl_object error;
        cl_env_ptr env = ecl_process_env();

        CL_CATCH_ALL_BEGIN(env) {
                error = ecl_make_symbol("ERROR", "CL");
                ECL_HANDLER_CASE_BEGIN(env, ecl_list1(error)) {
                        form = ecl_read_from_cstring(s);
                        val = cl_eval(1, form);
                        /* TODO: add the result to pool before returning it. */
                        ecl_return1(env, val);
                ECL_HANDLER_CASE(1, condition) {
                        /* FIXME: dunno what to do here (how do we get the actual error back out?) */
                        ecl_return1(env, OBJNULL);
                } ECL_HANDLER_CASE_END
        } CL_CATCH_ALL_IF_CAUGHT {
                ecl_return1(env, OBJNULL);
        } CL_CATCH_ALL_END
}

/* TODO: add out-of-band error parameter. */
cl_object read(const char *s, cl_object pool)
{
        cl_object form;
        cl_env_ptr env = ecl_process_env();

        CL_CATCH_ALL_BEGIN(env) {
                error = ecl_make_symbol("ERROR", "CL");
                ECL_HANDLER_CASE_BEGIN(env, ecl_list1(error)) {
                        form = ecl_read_from_cstring(s);
                        /* TODO: add form to pool before returning it. */
                        ecl_return1(env, form);
                } ECL_HANDLER_CASE(1, condition) {
                        /* FIXME: need to get the actual error info out somehow. */
                        ecl_return1(env, OBJNULL);
                } ECL_HANDLER_CASE_END
        } CL_CATCH_ALL_IF_CAUGHT {
                ecl_return1(env, OBJNULL);
        } CL_CATCH_ALL_END
}

/* FIXME: no room for out-of-band error-handling param (!) */
cl_object call(int nargs, cl_object func, cl_object arg, ...)
{
        cl_object apply;
        cl_object error;
        cl_object nreverse;
        cl_object arglist = arg;
        ecl_va_list varargs;
        cl_env_ptr env = ecl_process_env();
        int i;

        CL_CATCH_ALL_BEGIN(env) {
                error = ecl_make_symbol("ERROR", "CL");
                ECL_HANDLER_CASE_BEGIN(env, ecl_list1(error)) {
                        /* collect up a (lisp) list of arguments. */
                        ecl_va_start(varargs, arg, nargs, 2);
                        for (i = 0; i < nargs - 2; i++) {
                                arglist = cl_cons(ecl_va_arg(arglist), arglist);
                        }

                        nreverse = ecl_make_symbol("NREVERSE", "CL");
                        arglist = cl_funcall(2, nreverse, arglist);

                        apply = ecl_make_symbol("APPLY", "CL");
                        ecl_return1(env, cl_funcall(2, apply, arglist);
                } ECL_HANDLER_CASE(1, condition) {
                        /* FIXME: need to get the actual error info out somehow. */
                        ecl_return1(env, OBJNULL);
                } ECL_HANDLER_CASE_END
        CL_CATCH_ALL_IF_CAUGHT {
                ecl_return1(env, OBJNULL);
        } CL_CATCH_ALL_END
}
