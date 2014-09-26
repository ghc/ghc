#ifndef WIN32_SEH_EXCN_H
#define WIN32_SEH_EXCN_H

#include <stdio.h>
#include <stdlib.h>

#if defined(__MINGW32__)
/* Stuff needed to install and use SEH exception handlers */
#include <excpt.h>
#include <setjmp.h>
#include <windows.h>
#elif defined(_MSC_VER)
#include <windows.h>
#else
#include <signal.h>
#endif

/* Exception handling.
 *
 * On Win32, the default action for things like division by zero and
 * segfaults is to pop up an annoying little dialog box.
 *
 * This is a pain when we are SSHed into a Windows machine, or when we
 * want to debug a problem with gdb.
 *
 * seh_excn provides two macros, BEGIN_CATCH and END_CATCH, which
 * will catch such exceptions in the code they bracket and die by
 * printing a message and calling stg_exit(1).
 */
#define ON_DIV_ZERO fprintf(stdout,"divide by zero\n"); fflush(stdout);stg_exit(1)
#define ON_STACK_OVERFLOW fprintf(stdout,"C stack overflow in generated code\n"); fflush(stdout); stg_exit(1)
#define ON_SIGSEGV fprintf(stdout,"Segmentation fault/access violation in generated code\n"); fflush(stdout); stg_exit(1)

#if defined(__MINGW32__)
extern jmp_buf seh_unwind_to;
extern unsigned long seh_excn_code;
/*
 * install an exception handler 'exHandler' which longjmp()s (via 'jumpBuf')
 * to the code 'onExnCaught' when successfully catching an exception.
 *
 * Macro based on Andrew Begel's SEH support code posted to the mingw-users
 * mailing list.
 */
#define TRY_BEGIN(jumpBuf, exHandler, onExcnCaught)  \
    do { \
      int signal; \
      if ((signal = setjmp(jumpBuf)) != 0) { \
        onExcnCaught; \
      } else { \
        __try1(exHandler); \
      } \
    } while (0);

#define TRY_END()  __except1

extern
EXCEPTION_DISPOSITION
catchDivZero(struct _EXCEPTION_RECORD*,
         void*,
         struct _CONTEXT*,
         void*);

#define ON_EXCN \
   if (seh_excn_code == 1) { \
      ON_STACK_OVERFLOW; \
   } else if ( seh_excn_code == 2 ) { \
      ON_SIGSEGV; \
   } else { \
      ON_DIV_ZERO; \
   }

#define BEGIN_CATCH TRY_BEGIN(seh_unwind_to, catchDivZero, ON_EXCN)
#define END_CATCH TRY_END()
#elif defined(_MSC_VER)
#define BEGIN_CATCH __try {
#define END_CATCH   } __except ( ( ((GetExceptionCode() == EXCEPTION_FLT_DIVIDE_BY_ZERO) || (GetExceptionCode() == EXCEPTION_INT_DIVIDE_BY_ZERO) || (GetExceptionCode() == EXCEPTION_STACK_OVERFLOW) || (GetExceptionCode() == EXCEPTION_ACCESS_VIOLATION)) ? EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH ) ) { \
   switch ( (GetExceptionCode()) ) { \
    case EXCEPTION_FLT_DIVIDE_BY_ZERO: \
    case EXCEPTION_INT_DIVIDE_BY_ZERO:
          ON_DIV_ZERO; break; \
    case EXCEPTION_STACK_OVERFLOW: \
        ON_STACK_OVERFLOW; break; \
    case EXCEPTION_ACCESS_VIOLATION: \
        ON_SIGSEGV; break; \
   } \
  }
#else
#error Cannot determine what sort of Windows system this is
#endif

#endif /* WIN32_SEH_EXCN_H */

