#ifndef _MY_ERRORS_H
#define _MY_ERRORS_H

#include <windows.h>

/* There's two ways we can generate error messages - with different tradeoffs:
 * If we do a function call, we have to use a static buffer.
 * If we use a macro and ANSI C's string splicing, we have to use constant
 * strings - and accept a certain amount of overhead from inserting the
 * boilerplate text.
 */

/* result should be freed using LocalFree */
extern LPTSTR getErrorMessage(DWORD err);

#endif /* _MY_ERRORS_H */
