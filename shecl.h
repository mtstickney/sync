#ifndef SHECL_H
#define SHECL_H

#include <ecl.h>
#include <stdint.h>

/* Consumers of the dll can use this header to import the relevant symbols,
 * and we can use it to export them. */
#ifdef _EXPORT
#define SHECL_API __declspec(dllexport)
#else
#define SHECL_API __declspec(dllimport)
#endif

#ifdef __cplusplus
extern "C" {
#endif

int SHECL_API shecl_boot(char *bootstrap_fasl_path, int argc, char **argv);
void SHECL_API shecl_shutdown(void);

/* Protocol: functions return (VALUES value &optional error-msg). Use Lisp for multiple-values stuff.
 * If there was an error and we've returned two values, the primary value may be the condition object
 * that was signaled. */

cl_object SHECL_API eval(const char *s, cl_object pool);
cl_object SHECL_API read(const char *s, cl_object pool);

/* APPLY func to the arguments. There must be at least one other argument, a spreadable list of args. */
cl_object SHECL_API call(int nargs, cl_object pool, cl_object func, cl_object arg, ...);

/* Wrappers for macros that fetch multiple return values. */
cl_index SHECL_API shecl_nvalues(cl_env_ptr env);
cl_object SHECL_API shecl_nth_value(cl_env_ptr env, int n);

/* Type conversion functions. */
cl_object SHECL_API lisp_string(cl_object pool, char *str);
int SHECL_API string_p(cl_object obj);
char SHECL_API *c_string(cl_object obj);

cl_object SHECL_API lisp_double(cl_object pool, double d);
int SHECL_API double_p(cl_object obj);
int SHECL_API c_double(cl_object obj, double *d);

cl_object SHECL_API lisp_int64(cl_object pool, int64_t i);
int SHECL_API int64_p(cl_object obj);
int SHECL_API c_int64(cl_object obj, int64_t *i);

cl_object SHECL_API lisp_int(cl_object pool, int32_t i);
int SHECL_API int_p(cl_object obj);
int SHECL_API c_int(cl_object obj, int32_t *i);

cl_object SHECL_API lisp_bool(cl_object pool, int32_t b);
int SHECL_API bool_p(cl_object obj);
int SHECL_API c_bool(cl_object obj, int *b);
int SHECL_API c_generalized_bool(cl_object, int *b);

#ifdef __cplusplus
}
#endif

#endif
