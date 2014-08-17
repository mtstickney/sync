#include "shecl.h"

/* At one time this function did something useful. It might again someday, so leave it here. */
int shecl_boot(char *shecl_fasl_path, int argc, char **argv)
{
        cl_env_ptr env;

        cl_boot(argc, argv);
        env = ecl_process_env();

        /* Load the lisp-side code. */
        ECL_CATCH_ALL_BEGIN(env) {
                cl_object error = ecl_make_symbol("ERROR", "CL");
                ECL_HANDLER_CASE_BEGIN(env, ecl_list1(error)) {
                        cl_object load = ecl_make_symbol("LOAD", "CL");

                        cl_funcall(2, load, ecl_cstring_to_base_string_or_nil(shecl_fasl_path));
                } ECL_HANDLER_CASE(1, condition) {
                        /* ECL_HANDLER_CASE_BEGIN(env, ecl_list1(error)) { */
                        /*         cl_object error_string; */
                        /*         cl_object write_to_string = ecl_make_symbol("WRITE-TO-STRING", "CL"); */
                        /*         cl_object escape = ecl_make_keyword("ESCAPE"); */
                        /*         error_string = cl_funcall(3, write_to_string, condition, escape, ECL_NIL); */
                        /*         ecl_return2(env, condition, error_string); */
                        /* } ECL_HANDLER_CASE(1, condition2) { */
                        /*         ecl_return2(env, condition, ecl_cstring_to_base_string_or_nil("Error loading shecl system.")); */
                        /* } ECL_HANDLER_CASE_END; */
                        return -1;
                } ECL_HANDLER_CASE_END;
        } ECL_CATCH_ALL_IF_CAUGHT {
                cl_shutdown();
                return -1;
        } ECL_CATCH_ALL_END;

        /* TODO: load the lisp-side shecl system here (or use a system dll or something). */
        return 0;
}

void shecl_shutdown(void)
{
        cl_shutdown();
}

cl_object eval(const char *s, cl_object pool)
{
        cl_env_ptr env = ecl_process_env();

        ECL_CATCH_ALL_BEGIN(env) {
                cl_object safe_eval_string = ecl_make_symbol("SAFE-EVAL-STRING", "SHECL");
                cl_object pool_var = ecl_make_symbol("*POOL*", "SHECL");
                cl_object val;

                if (pool != OBJNULL)
                        ecl_bds_bind(env, pool_var, pool);
                ECL_UNWIND_PROTECT_BEGIN(env) {
                        /* Note that cl_funcall handles the multiple-value magic. */
                        val = cl_funcall(2, safe_eval_string, ecl_cstring_to_base_string_or_nil(s));
                } ECL_UNWIND_PROTECT_EXIT {
                        if (pool != OBJNULL)
                                ecl_bds_unwind1(env);
                } ECL_UNWIND_PROTECT_END;

                /* Note that we're deliberately not using ecl_returnX(): cl_funcall has already
                 * set up the return values in env, we just have to get the primary one out.
                 * This may cause problems if ecl_returnX() changes in the future. */
                return val;
        } ECL_CATCH_ALL_IF_CAUGHT {
                ecl_return2(env, OBJNULL, OBJNULL);
        } ECL_CATCH_ALL_END;
}

cl_object read(const char *s, cl_object pool)
{
        cl_env_ptr env = ecl_process_env();

        ECL_CATCH_ALL_BEGIN(env) {
                cl_object safe_read_from_string = ecl_make_symbol("SAFE-READ-FROM-STRING", "SHECL");
                cl_object pool_var = ecl_make_symbol("*POOL*", "SHECL");
                cl_object val;

                if (pool != OBJNULL)
                        ecl_bds_bind(env, pool_var, pool);
                ECL_UNWIND_PROTECT_BEGIN(env) {
                        /* cl_funcall will handle the multiple-value magic. */
                        val = cl_funcall(2, safe_read_from_string, ecl_cstring_to_base_string_or_nil(s));
                } ECL_UNWIND_PROTECT_EXIT {
                        if (pool != OBJNULL)
                                ecl_bds_unwind1(env);
                } ECL_UNWIND_PROTECT_END;

                /* Note that since cl_funcall has already set up the env we just need to get
                 * the primary one out. Note that this may break if ecl_returnX() changes in the future. */
                return val;
        } ECL_CATCH_ALL_IF_CAUGHT {
                ecl_return2(env, OBJNULL, OBJNULL);
        } ECL_CATCH_ALL_END;
}

/* Poor man's substitute for (apply #'list arg1 arg2 ...) -- including spread behavior of the last arg. */
cl_object varglist(int nvargs, cl_object fixedarg, ecl_va_list varargs)
{
        cl_env_ptr env = ecl_process_env();
        cl_object arglist;
        cl_object lastcons;
        int i;

        if (nvargs = 0) {
                ecl_return1(env, fixedarg);
        } else {
                arglist = ecl_list1(fixedarg);
                lastcons = arglist;
        }

        for (i = 0; i < nvargs; i++) {
                cl_object arg = ecl_va_arg(varargs);

                if (i < nvargs - 1)
                        /* The last argument is already a list, otherwise make a cons. */
                        arg = cl_cons(arg, ECL_NIL);

                cl_rplacd(lastcons, arg);
                lastcons = arg;
        }
        ecl_return1(env, arglist);
}

cl_object call(int nargs, cl_object pool, cl_object func, cl_object arg, ...)
{
        cl_env_ptr env = ecl_process_env();
        cl_object val;

        ECL_CATCH_ALL_BEGIN(env) {
                cl_object error = ecl_make_symbol("ERROR", "CL");
                ECL_HANDLER_CASE_BEGIN(env, ecl_list1(error)) {
                        int i;
                        ecl_va_list varargs;
                        cl_object safe_apply = ecl_make_symbol("SAFE-APPLY", "SHECL");
                        cl_object pool_var = ecl_make_symbol("*POOL*", "SHECL");

                        if (pool != OBJNULL)
                                ecl_bds_bind(env, pool_var, pool);
                        ECL_UNWIND_PROTECT_BEGIN(env) {
                                cl_object arglist;

                                /* collect up a (lisp) list of arguments. */
                                ecl_va_start(varargs, arg, nargs, 3);
                                arglist = varglist(nargs - 3, arg, varargs);
                                ecl_va_end(varargs);

                                /* Note that cl_funcall handles the multiple-values magic. */
                                val = cl_funcall(2, safe_apply, arglist);
                        } ECL_UNWIND_PROTECT_EXIT {
                                if (pool != OBJNULL)
                                        ecl_bds_unwind1(env);
                        } ECL_UNWIND_PROTECT_END;
                        /* Since cl_funcall handled the multiple-value business, just get the primary out.
                         * Depends on the internals of ecl_returnX() a bit, so it's fragile. */
                        return val;
                } ECL_HANDLER_CASE(1, condition) {
                        ecl_return2(env, condition, ecl_cstring_to_base_string_or_nil("Error constructing function call"));
                } ECL_HANDLER_CASE_END;
        } ECL_CATCH_ALL_IF_CAUGHT {
                ecl_return2(env, OBJNULL, OBJNULL);
        } ECL_CATCH_ALL_END;
}
