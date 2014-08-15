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
        cl_env_ptr env = ecl_process_env();

        CL_CATCH_ALL_BEGIN(env) {
                cl_object safe_eval_string = ecl_make_symbol("SAFE-EVAL-STRING", "SHECL");

                /* TODO: add value to pool before returning it. */
                return cl_funcall(2, safe_eval_string, ecl_cstring_to_base_string_or_nil(s));
        } CL_CATCH_ALL_IF_CAUGHT {
                ecl_return1(env, OBJNULL);
        } CL_CATCH_ALL_END
}

/* TODO: add out-of-band error parameter. */
cl_object read(const char *s, cl_object pool)
{
        cl_env_ptr env = ecl_process_env();

        CL_CATCH_ALL_BEGIN(env) {
                cl_object safe_read_from_string = ecl_make_symbol("SAFE-READ-FROM-STRING", "SHECL");
                /* TODO: add form to pool before returning it. */
                return cl_funcall(2, safe_read_from_string, ecl_cstring_to_base_string_or_nil(s));
        } CL_CATCH_ALL_IF_CAUGHT {
                ecl_return1(env, OBJNULL);
        } CL_CATCH_ALL_END
}

/* FIXME: no room for out-of-band error-handling param (!) */
cl_object call(int nargs, cl_object func, cl_object arg, ...)
{
        cl_env_ptr env = ecl_process_env();

        CL_CATCH_ALL_BEGIN(env) {
                cl_object error = ecl_make_symbol("ERROR", "CL");
                ECL_HANDLER_CASE_BEGIN(env, ecl_list1(error)) {
                        int i;
                        ecl_va_list varargs;
                        cl_object arglist = arg;
                        cl_object nreverse = ecl_make_symbol("NREVERSE", "CL");
                        cl_object safe_apply = ecl_make_symbol("SAFE-APPLY", "SHECL");

                        /* collect up a (lisp) list of arguments. */
                        ecl_va_start(varargs, arg, nargs, 2);
                        for (i = 0; i < nargs - 2; i++) {
                                arglist = cl_cons(ecl_va_arg(arglist), arglist);
                        }
                        ecl_va_end(varargs);

                        arglist = cl_funcall(2, nreverse, arglist);
                        return cl_funcall(2, safe_apply, arglist);
                } ECL_HANDLER_CASE(1, condition) {
                        /* FIXME: need to get the actual error info out somehow. */
                        ecl_return1(env, OBJNULL);
                } ECL_HANDLER_CASE_END
        CL_CATCH_ALL_IF_CAUGHT {
                ecl_return1(env, OBJNULL);
        } CL_CATCH_ALL_END
}
