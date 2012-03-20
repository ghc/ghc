#include "ghcconfig.h"
#include "seh_excn.h"

/*
 * Exception / signal handlers.
 */
#if defined(mingw32_HOST_OS)
#if defined(i386_HOST_ARCH)
jmp_buf seh_unwind_to;
unsigned long seh_excn_code; /* variable used to communicate what kind of exception we've caught;nice. */

EXCEPTION_DISPOSITION
catchDivZero(struct _EXCEPTION_RECORD* rec,
         void* arg1 __attribute__((unused)),
         struct _CONTEXT* ctxt __attribute__((unused)),
         void* arg2 __attribute__((unused)))
{
     if ((rec->ExceptionFlags & EH_UNWINDING) != 0) {
     // When the system unwinds the SEH stack after having handled an excn,
     // return immediately.
         return ExceptionContinueSearch;
     }
     switch (rec->ExceptionCode) {
     case EXCEPTION_FLT_DIVIDE_BY_ZERO:
     case EXCEPTION_INT_DIVIDE_BY_ZERO:
     seh_excn_code = 0;
     longjmp(seh_unwind_to, rec->ExceptionCode);
     return ExceptionContinueExecution;
     case EXCEPTION_STACK_OVERFLOW:
     seh_excn_code = 1;
     longjmp(seh_unwind_to, rec->ExceptionCode);
     return ExceptionContinueExecution;
     case EXCEPTION_ACCESS_VIOLATION:
     seh_excn_code = 2;
     longjmp(seh_unwind_to, rec->ExceptionCode);
     return ExceptionContinueExecution;
     longjmp(seh_unwind_to, rec->ExceptionCode);
     return ExceptionContinueExecution;
     default: ;
     }
     return ExceptionContinueSearch;
}
#endif
#endif

