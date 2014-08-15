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
                        return val;
                ECL_HANDLER_CASE(1, condition) {
                        /* FIXME: dunno what to do here (how do we get the actual error back out?) */
                        return OBJNULL;
                } ECL_HANDLER_CASE_END
        } CL_CATCH_ALL_IF_CAUGHT {
                return OBJNULL;
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
                        return form;
                } ECL_HANDLER_CASE(1, condition) {
                        /* FIXME: need to get the actual error info out somehow. */
                        return OBJNULL;
                } ECL_HANDLER_CASE_END
        } CL_CATCH_ALL_IF_CAUGHT {
                return OBJNULL;
        } CL_CATCH_ALL_END
}
