#ifndef SHECL_H
#define #SHECL_H

#include <ecl.h>

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

int SHECL_API shecl_boot(int argc, char **argv);
void SHECL_API shecl_shutdown(void);

/* Protocol: functions return (VALUES value &optional error-msg). Use Lisp for multiple-values stuff.
 * If there was an error and we've returned two values, the primary value may be the condition object
 * that was signaled. */

cl_object SHECL_API eval(const char *s, cl_object pool);
cl_object SHECL_API read(const char *s, cl_object pool);

/* APPLY func to the arguments. There must be at least one other argument, a spreadable list of args. */
cl_object SHECL_API call(int nargs, cl_object pool, cl_object func, cl_object arg, ...);

#ifdef __cplusplus
}
#endif

#endif