#ifndef SHECL_H
#define SHECL_H

#include <ecl.h>
#include <stdint.h>

/* Consumers of the dll can use this header to import the relevant symbols,
 * and we can use it to export them. */
#ifdef _EXPORTING
#define SHECL_API __declspec(dllexport)
#else
#define SHECL_API __declspec(dllimport)
#endif

#ifdef __cplusplus
extern "C" {
#endif

int SHECL_API shecl_boot(char *shecl_fasl_path, int argc, char **argv);
void SHECL_API shecl_shutdown(void);

/* Protocol: functions return (VALUES value &optional error-msg). Use Lisp for multiple-values stuff.
 * If there was an error and we've returned two values, the primary value may be the condition object
 * that was signaled. */

cl_object SHECL_API eval(const char *s, cl_object pool);
cl_object SHECL_API read(const char *s, cl_object pool);

/* APPLY func to the arguments. There must be at least one other argument, a spreadable list of args. */
cl_object SHECL_API call(int nargs, cl_object pool, cl_object func, cl_object arg, ...);

/* Type conversion functions. */
cl_object SHECL_API lisp_string(char *str);
int SHECL_API string_p(cl_object obj);

cl_object SHECL_API lisp_double(double d);
int SHECL_API double_p(cl_object obj);
int SHECL_API c_double(cl_object obj, double *d);

cl_object SHECL_API lisp_int64(int64_t i);
int SHECL_API int64_p(cl_object obj);
int SHECL_API c_int64(cl_object obj, int64_t *i);

cl_object SHECL_API lisp_int(int32_t i);
int SHECL_API int_p(cl_object obj);
int SHECL_API c_int(cl_object obj, int32_t *i);

cl_object SHECL_API lisp_bool(int32_t b);
int SHECL_API bool_p(cl_object obj);
int SHECL_API c_bool(cl_object obj, int *b);
int SHECL_API c_generalized_bool(cl_object, int *b);

#ifdef __cplusplus
}
#endif

#endif
