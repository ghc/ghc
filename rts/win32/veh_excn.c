/* -----------------------------------------------------------------------------
*
* (c) The GHC Team 1998-2000
*
* Error Handling implementations for windows
*
* ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "ghcconfig.h"
#include "veh_excn.h"
#include <assert.h>

/////////////////////////////////
// Exception / signal handlers.
/////////////////////////////////

// Define some values for the ordering of VEH Handlers:
// - CALL_FIRST means call this exception handler first
// - CALL_LAST means call this exception handler last
#define CALL_FIRST 1
#define CALL_LAST 0

// this should be in <excpt.h>, but it's been removed from MinGW distributions
#ifndef EH_UNWINDING
#define EH_UNWINDING   0x02
#endif /* EH_UNWINDING */

// Registered exception handler
PVOID __hs_handle = NULL;

long WINAPI __hs_exception_handler(struct _EXCEPTION_POINTERS *exception_data)
{
    long action = EXCEPTION_CONTINUE_SEARCH;

    // When the system unwinds the VEH stack after having handled an excn,
    // return immediately.
    if ((exception_data->ExceptionRecord->ExceptionFlags & EH_UNWINDING) == 0)
    {
        // Error handling cases covered by this implementation.
        switch (exception_data->ExceptionRecord->ExceptionCode) {
            case EXCEPTION_FLT_DIVIDE_BY_ZERO:
            case EXCEPTION_INT_DIVIDE_BY_ZERO:
                fprintf(stdout, "divide by zero\n");
                action = EXCEPTION_CONTINUE_EXECUTION;
                break;
            case EXCEPTION_STACK_OVERFLOW:
                fprintf(stdout, "C stack overflow in generated code\n");
                action = EXCEPTION_CONTINUE_EXECUTION;
                break;
            case EXCEPTION_ACCESS_VIOLATION:
                fprintf(stdout, "Segmentation fault/access violation in generated code\n");
                action = EXCEPTION_CONTINUE_EXECUTION;
                break;
            default:;
        }

        // If an error has occurred and we've decided to continue execution
        // then we've done so to prevent something else from handling the error.
        // But the correct action is still to exit as fast as possible.
        if (EXCEPTION_CONTINUE_EXECUTION == action)
        {
            fflush(stdout);
            stg_exit(EXIT_FAILURE);
        }
    }

    return action;
}

void __register_hs_exception_handler( void )
{
    // Allow the VEH handler to be registered only once.
    if (NULL == __hs_handle)
    {
        __hs_handle = AddVectoredExceptionHandler(CALL_FIRST, __hs_exception_handler);
        // should the handler not be registered this will return a null.
        assert(__hs_handle);
    }
    else
    {
        errorBelch("There is no need to call __register_hs_exception_handler() twice, VEH handlers are global per process.");
    }
}

void __unregister_hs_exception_handler( void )
{
    if (__hs_handle != NULL)
    {
        // Should the return value be checked? we're terminating anyway.
        RemoveVectoredExceptionHandler(__hs_handle);
        __hs_handle = NULL;
    }
    else
    {
        errorBelch("__unregister_hs_exception_handler() called without having called __register_hs_exception_handler() first.");
    }
}

