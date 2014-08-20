#include "shecl.h"

cl_object report_error(cl_env_ptr env, cl_object condition, char *msg) {
        cl_object error = ecl_make_symbol("SERIOUS-CONDITION", "CL");
        ECL_HANDLER_CASE_BEGIN(env, ecl_list1(error)) {
                cl_object error_string;
                cl_object write_to_string = ecl_make_symbol("WRITE-TO-STRING", "CL");
                cl_object escape = ecl_make_keyword("ESCAPE");
                error_string = cl_funcall(3, write_to_string, condition, escape, ECL_NIL);
                ecl_return2(env, condition, error_string);
        } ECL_HANDLER_CASE(1, condition2) {
        } ECL_HANDLER_CASE_END;
        ecl_return2(env, condition, ecl_cstring_to_base_string_or_nil(msg));
}

/* At one time this function did something useful. It might again someday, so leave it here. */
int shecl_boot(char *bootstrap_fasl_path, int argc, char **argv)
{
        cl_env_ptr env;

        cl_boot(argc, argv);
        env = ecl_process_env();

        /* Load the lisp-side code. */
        ECL_CATCH_ALL_BEGIN(env) {
                cl_object error = ecl_make_symbol("SERIOUS-CONDITION", "CL");
                ECL_HANDLER_CASE_BEGIN(env, ecl_list1(error)) {
                        cl_object load = ecl_make_symbol("LOAD", "CL");
                        cl_object fasl_path_str;
                        cl_object bootstrap;

                        fasl_path_str = ecl_cstring_to_base_string_or_nil(bootstrap_fasl_path);
                        cl_funcall(2, load, fasl_path_str);
                        bootstrap = ecl_make_symbol("BOOTSTRAP", "SHECL-BOOTSTRAP");
                        cl_funcall(2, bootstrap, fasl_path_str);
                        return 0;
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
                        cl_shutdown();
                        return -1;
                } ECL_HANDLER_CASE_END;
        } ECL_CATCH_ALL_END;
        /* We've already returned on success, so if we got here there must have been an error. */
        cl_shutdown();
        return -1;
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
        } ECL_CATCH_ALL_END;
        ecl_return2(env, OBJNULL, OBJNULL);
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
        } ECL_CATCH_ALL_END;
        ecl_return2(env, OBJNULL, OBJNULL);
}

/* Poor man's substitute for (apply #'list arg1 arg2 ...) -- including spread behavior of the last arg. */
cl_object varglist(int nvargs, cl_object fixedarg, ecl_va_list varargs)
{
        cl_env_ptr env = ecl_process_env();
        cl_object arglist;
        cl_object lastcons;
        int i;

        if (nvargs == 0) {
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
                cl_object error = ecl_make_symbol("SERIOUS-CONDITION", "CL");
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
                                val = cl_funcall(3, safe_apply, func, arglist);
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
        } ECL_CATCH_ALL_END;
        ecl_return2(env, OBJNULL, OBJNULL);
}

cl_index shecl_nvalues(cl_env_ptr env)
{
        return ecl_nvalues(env);
}

cl_object SHECL_API shecl_nth_value(cl_env_ptr env, int n)
{
        /* FIXME: might be bad that this doesn't set env properly (handy for looping, bad for calling this like the lisp function) */
        return ecl_nth_value(env, n);
}

/* Type conversion functions */

int shecl_typep(cl_object obj, cl_object type)
{
        cl_env_ptr env = ecl_process_env();
        ECL_CATCH_ALL_BEGIN(env) {
                cl_object serious_condition = ecl_make_symbol("SERIOUS-CONDITION", "CL");
                ECL_HANDLER_CASE_BEGIN(env, ecl_list1(serious_condition)) {
                        cl_object typep = ecl_make_symbol("TYPEP", "CL");
                        cl_object ret = cl_funcall(3, typep, obj, type);
                        if (ret == ECL_NIL)
                                return 0;
                        return 1;
                } ECL_HANDLER_CASE(1, condition) {
                        return -1;
                } ECL_HANDLER_CASE_END;
        } ECL_CATCH_ALL_END;
        return -1;
}

cl_object signed_byte_type(unsigned int nbits)
{
        cl_object signed_byte = ecl_make_symbol("SIGNED-BYTE", "CL");
        cl_object bits = ecl_make_integer(nbits);
        return cl_list(2, signed_byte, bits);
}

/* CHARACTER type */
cl_object lisp_string(cl_object pool, char *str)
{
        cl_env_ptr env = ecl_process_env();
        ECL_CATCH_ALL_BEGIN(env) {
                cl_object serious_condition = ecl_make_symbol("SERIOUS-CONDITION", "CL");
                ECL_HANDLER_CASE_BEGIN(env, ecl_list1(serious_condition)) {
                        cl_object add_to_pool = ecl_make_symbol("ADD-TO-POOL", "SHECL");
                        cl_object string = ecl_cstring_to_base_string_or_nil(str);
                        if (string == ECL_NIL)
                                cl_error(1, ecl_cstring_to_base_string_or_nil("String data was NULL."));
                        ecl_return1(env, cl_funcall(3, add_to_pool, string, pool));
                } ECL_HANDLER_CASE(1, condition) {
                        return report_error(env, condition, "Error constructing Lisp string.");
                } ECL_HANDLER_CASE_END;
        } ECL_CATCH_ALL_END;
        ecl_return2(env, OBJNULL, OBJNULL);
}

int string_p(cl_object obj)
{
        cl_env_ptr env = ecl_process_env();
        ECL_CATCH_ALL_BEGIN(env) {
                cl_object serious_condition = ecl_make_symbol("SERIOUS-CONDITION", "CL");
                ECL_HANDLER_CASE_BEGIN(env, ecl_list1(serious_condition)) {
                        cl_object result = cl_stringp(obj);
                        if (result == ECL_NIL)
                                return 1;
                        return 0;
                } ECL_HANDLER_CASE(1, condition) {
                        return -1;
                } ECL_HANDLER_CASE_END;
        } ECL_CATCH_ALL_END;
        return -1;
}

char *c_string(cl_object obj)
{
        cl_env_ptr env = ecl_process_env();
        char *data;
        char *str;
        ECL_CATCH_ALL_BEGIN(env) {
                cl_object serious_condition = ecl_make_symbol("SERIOUS-CONDITION", "CL");
                ECL_HANDLER_CASE_BEGIN(env, ecl_list1(serious_condition)) {
                        cl_object foreign_string_alloc = ecl_make_symbol("FOREIGN-STRING-ALLOC", "CFFI");
                        cl_object foreign_string_free = ecl_make_symbol("FOREIGN-STRING-FREE", "CFFI");
                        cl_object encoding_kw = ecl_make_keyword("ENCODING");
                        cl_object encoding = ecl_make_keyword("UTF-8");
                        cl_object pointer_address = ecl_make_symbol("POINTER-ADDRESS", "CFFI");
                        cl_object foreign_string = cl_funcall(2, foreign_string_alloc, obj);
                        cl_object ptr;
                        size_t len;
                        ECL_UNWIND_PROTECT_BEGIN(env) {
                                ptr = cl_funcall(2, pointer_address, foreign_string);
                                data = (char*)ecl_to_unsigned_integer(ptr);
                                len = strlen(data);
                                str = malloc(len);
                                if (str)
                                        /* Hooray for overflows! */
                                        strcpy(str, data);
                        } ECL_UNWIND_PROTECT_EXIT {
                                cl_funcall(2, foreign_string_free, foreign_string);
                        } ECL_UNWIND_PROTECT_END;
                        return str;
                } ECL_HANDLER_CASE(1, condition) {
                        return NULL;
                } ECL_HANDLER_CASE_END;
        } ECL_CATCH_ALL_END;
        return NULL;
}

/* DECIMAL type */
cl_object lisp_double(cl_object pool, double d)
{
        cl_env_ptr env = ecl_process_env();
        ECL_CATCH_ALL_BEGIN(env) {
                cl_object serious_condition = ecl_make_symbol("SERIOUS-CONDITION", "CL");
                ECL_HANDLER_CASE_BEGIN(env, ecl_list1(serious_condition)) {
                        cl_object add_to_pool = ecl_make_symbol("ADD-TO-POOL", "SHECL");
                        cl_object new_double = ecl_make_double_float(d);
                        /* DOn't bother with the pool if it's an immediate type. */
                        if (ECL_IMMEDIATE(new_double))
                                ecl_return1(env, new_double);
                        ecl_return1(env, cl_funcall(3, add_to_pool, new_double, pool));
                } ECL_HANDLER_CASE(1, condition) {
                        return report_error(env, condition, "Error creating lisp double object.");
                } ECL_HANDLER_CASE_END;
        } ECL_CATCH_ALL_END;
        ecl_return2(env, OBJNULL, OBJNULL);
}

int double_p(cl_object obj)
{
        cl_env_ptr env = ecl_process_env();
        ECL_CATCH_ALL_BEGIN(env) {
                cl_object type = ecl_make_symbol("DOUBLE-FLOAT", "CL");
                return shecl_typep(obj, type);
        } ECL_CATCH_ALL_END;
        return -1;
}

int c_double(cl_object obj, double *d)
{
        cl_env_ptr env = ecl_process_env();
        ECL_CATCH_ALL_BEGIN(env) {
                *d = ecl_to_double(obj);
                return 0;
        } ECL_CATCH_ALL_END;
        return -1;
}

/* INT64 type */

cl_object lisp_int64(cl_object pool, int64_t i)
{
        cl_env_ptr env = ecl_process_env();
        ECL_CATCH_ALL_BEGIN(env) {
                cl_object serious_condition = ecl_make_symbol("SERIOUS-CONDITION", "CL");
                ECL_HANDLER_CASE_BEGIN(env, ecl_list1(serious_condition)) {
                        cl_object add_to_pool = ecl_make_symbol("ADD-TO-POOL", "CL");
                        cl_object new_int64 = ecl_make_int64_t(i);
                        /* Don't bother with the pool if it's an immediate type. */
                        if (ECL_IMMEDIATE(new_int64))
                                ecl_return1(env, new_int64);
                        ecl_return1(env, cl_funcall(3, add_to_pool, new_int64, pool));
                } ECL_HANDLER_CASE(1, condition) {
                        return report_error(env, condition, "Error constructing lisp int64 object.");
                } ECL_HANDLER_CASE_END;
        } ECL_CATCH_ALL_END;
        ecl_return2(env, OBJNULL, OBJNULL);
}

int int64_p(cl_object obj)
{
        cl_env_ptr env = ecl_process_env();
        ECL_CATCH_ALL_BEGIN(env) {
                cl_object type = signed_byte_type(64);
                return shecl_typep(obj, type);
        } ECL_CATCH_ALL_END;
        return -1;
}

int c_int64(cl_object obj, int64_t *i)
{
        cl_env_ptr env = ecl_process_env();
        ECL_CATCH_ALL_BEGIN(env) {
                cl_object serious_condition = ecl_make_symbol("SERIOUS-CONDITION", "CL");
                ECL_HANDLER_CASE_BEGIN(env, ecl_list1(serious_condition)) {
                        *i = ecl_to_int64_t(obj);
                        return 0;
                } ECL_HANDLER_CASE(1, condition){
                        return -1;
                } ECL_HANDLER_CASE_END;
        } ECL_CATCH_ALL_END;
        return -1;
}

/* INTEGER type */
cl_object lisp_int(cl_object pool, int32_t i)
{
        cl_env_ptr env = ecl_process_env();
        ECL_CATCH_ALL_BEGIN(env) {
                cl_object serious_condition = ecl_make_symbol("SERIOUS-CONDITION", "CL");
                ECL_HANDLER_CASE_BEGIN(env, ecl_list1(serious_condition)) {
                        cl_object add_to_pool = ecl_make_symbol("ADD-TO-POOL", "SHECL");
                        cl_object new_int = ecl_make_int32_t(i);
                        /* Don't bother with the pool if it's an immediate type. */
                        if (ECL_IMMEDIATE(new_int))
                                ecl_return1(env, new_int);
                        ecl_return1(env, cl_funcall(3, add_to_pool, new_int, pool));
                } ECL_HANDLER_CASE(1, condition) {
                        return report_error(env, condition, "Error constructing lisp int32 object.");
                } ECL_HANDLER_CASE_END;
        } ECL_CATCH_ALL_END;
        ecl_return2(env, OBJNULL, OBJNULL);
}

int int_p(cl_object obj)
{
        cl_env_ptr env = ecl_process_env();
        ECL_CATCH_ALL_BEGIN(env) {
                cl_object type = signed_byte_type(32);
                return shecl_typep(obj, type);
        } ECL_CATCH_ALL_END;
        return -1;
}

int c_int(cl_object obj, int32_t *i)
{
        cl_env_ptr env = ecl_process_env();
        ECL_CATCH_ALL_BEGIN(env) {
                cl_object serious_condition = ecl_make_symbol("SERIOUS-CONDITION", "CL");
                ECL_HANDLER_CASE_BEGIN(env, ecl_list1(serious_condition)) {
                        *i = ecl_to_int32_t(obj);
                        return 0;
                } ECL_HANDLER_CASE(1, condition) {
                        return -1;
                } ECL_HANDLER_CASE_END;
        } ECL_CATCH_ALL_END;
        return -1;
}

/* LOGICAL type (note that we're using int as an intermediary). */
cl_object lisp_bool(cl_object pool, int32_t b)
{
        /* Note: since T and NIL will never be collected, a pool is
         * unecessary (and ignored) here. */
        cl_env_ptr env = ecl_process_env();
        ECL_CATCH_ALL_BEGIN(env) {
                if (b)
                        ecl_return1(env, ECL_T);
                else
                        ecl_return1(env, ECL_NIL);
        } ECL_CATCH_ALL_END;
        ecl_return2(env, OBJNULL, OBJNULL);
}

int bool_p(cl_object obj)
{
        cl_env_ptr env = ecl_process_env();
        ECL_CATCH_ALL_BEGIN(env) {
                if (obj == ECL_T || obj == ECL_NIL)
                        return 1;
                else
                        return 0;
        } ECL_CATCH_ALL_END;
        return -1;
}

int c_bool(cl_object obj, int *b)
{
        if (obj == ECL_T) {
                *b = 0;
                return 0;
        } else if (obj == ECL_NIL) {
                *b = 1;
                return 0;
        }
        return -1;
}

int c_generalized_bool(cl_object obj, int *b)
{
        if (obj == ECL_NIL) {
                *b = 0;
                return 0;
        } else {
                *b = 1;
                return 0;
        }
}
