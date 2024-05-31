/* -----------------------------------------------------------------------------
*
* (c) The GHC Team 1998-2000
*
* Error Handling implementations for windows
*
* ---------------------------------------------------------------------------*/
#define UNICODE 1
#include "Rts.h"
#include "ghcconfig.h"
#include "veh_excn.h"
#include "LinkerInternals.h"
#include <stdbool.h>
#include <dbghelp.h>
#include <shellapi.h>
#include <shlobj.h>
#include <wchar.h>
#include <windows.h>
#include <stdio.h>
#include <excpt.h>
#include <inttypes.h>
#include <dbghelp.h>
#include <signal.h>

/////////////////////////////////
// Exception / signal handlers.
/////////////////////////////////

/*
  SEH (Structured Error Handler) on Windows is quite tricky. On x86 SEHs are
  stack based and are stored in FS[0] of each thread. Which means every time we
  spawn an OS thread we'd have to set up the error handling. However on x64 it's
  table based and memory region based. e.g. you register a handler for a
  particular memory range. This means that we'd have to register handlers for
  each block of code we load externally or generate internally ourselves.

  In Windows XP VEH (Vectored Exception Handler) and VCH (Vectored Continue
  Handler) were added. Both of these are global/process wide handlers, the
  former handling all exceptions and the latter handling only exceptions which
  we're trying to recover from, e.g. a handler returned
  EXCEPTION_CONTINUE_EXECUTION.

  And lastly you have top level exception filters, which are also process global
  but the problem here is that you can only have one, and setting this removes
  the previous ones. The chain of exception handling looks like

                    [  Vectored Exception Handler  ]
                                |
                    [ Structured Exception Handler ]
                                |
                    [      Exception Filters       ]
                                |
                    [  Vectored Continue Handler   ]

  To make things more tricky, the exception handlers handle both hardware and
  software exceptions Which means previously when we registered VEH handlers
  we would also trap software exceptions. Which means when haskell code was
  loaded in a C++ or C# context we would swallow exceptions and terminate in
  contexts that normally the runtime should be able to continue on, e.g. you
  could be handling the segfault in your C++ code, or the div by 0.

  We could not handle these exceptions, but GHCi would just die a horrible death
  then on normal Haskell only code when such an exception occurs.

  So instead, we'll move to Continue handler, to run as late as possible, and
  also register a filter which calls any existing filter, and then runs the
  continue handlers, we then also only run as the last continue handler so we
  don't supersede any other VCH handlers.

  Lastly we'll also provide a way for users to disable the exception handling
  entirely so even if the new approach doesn't solve the issue they can work
  around it. After all, I don't expect any interpreted code if you are running
  a haskell dll.

  For a detailed analysis see
  https://reverseengineering.stackexchange.com/questions/14992/what-are-the-vectored-continue-handlers
  and https://www.gamekiller.net/threads/vectored-exception-handler.3237343/
  */

// Define some values for the ordering of VEH Handlers:
// - CALL_FIRST means call this exception handler first
// - CALL_LAST means call this exception handler last
#define CALL_FIRST 1
#define CALL_LAST 0

// this should be in <excpt.h>, but it's been removed from MinGW distributions
#if !defined(EH_UNWINDING)
#define EH_UNWINDING   0x02
#endif /* EH_UNWINDING */

// Registered exception handler
PVOID __hs_handle = NULL;
LPTOP_LEVEL_EXCEPTION_FILTER oldTopFilter = NULL;

long WINAPI __hs_exception_handler(struct _EXCEPTION_POINTERS *exception_data)
{
    long action   = EXCEPTION_CONTINUE_SEARCH;
    int exit_code = EXIT_FAILURE;
    ULONG_PTR what;
    fprintf (stderr, "\n");

    // When the system unwinds the VEH stack after having handled an excn,
    // return immediately.
    if (exception_data
        && exception_data->ExceptionRecord
        && (exception_data->ExceptionRecord->ExceptionFlags & EH_UNWINDING) ==0)
    {
        // Error handling cases covered by this implementation.
        switch (exception_data->ExceptionRecord->ExceptionCode) {
            case EXCEPTION_FLT_DIVIDE_BY_ZERO:
            case EXCEPTION_INT_DIVIDE_BY_ZERO:
                fprintf(stderr, "divide by zero\n");
                action = EXCEPTION_CONTINUE_EXECUTION;
                exit_code = SIGFPE;
                break;
            case EXCEPTION_STACK_OVERFLOW:
                fprintf(stderr, "C stack overflow in generated code\n");
                action = EXCEPTION_CONTINUE_EXECUTION;
                break;
            case EXCEPTION_ACCESS_VIOLATION:
              {
                if (exception_data->ExceptionRecord->NumberParameters < 2)
                  {
                    fprintf(stderr, "Access violation in generated code. "
                                    "Empty exception record.");
                  }
                else
                  {
                    what = exception_data->ExceptionRecord
                                         ->ExceptionInformation[0];
                    fprintf(stderr, "Access violation in generated code"
                                    " when %s 0x%" PRIxPTR "\n"
                                    , what == 0 ? "reading"
                                    : what == 1 ? "writing"
                                    : what == 8 ? "executing data at"
                                    :             "?"
                                    , (uintptr_t) exception_data
                                                    ->ExceptionRecord
                                                    ->ExceptionInformation[1]
                                );
                  }
                action = EXCEPTION_CONTINUE_EXECUTION;
                exit_code = SIGSEGV;
                break;
              }
            default:;
        }

        // If an error has occurred and we've decided to continue execution
        // then we've done so to prevent something else from handling the error.
        // But the correct action is still to exit as fast as possible.
        if (EXCEPTION_CONTINUE_EXECUTION == action)
        {
            fflush(stderr);
            hs_restoreConsoleCP ();
            generateStack (exception_data);
            generateDump (exception_data);
            stg_exit(exit_code);
        }
    }

    return action;
}

/* Registered top level exception filter.  We're not very interested in handling
   the error here, that's why we have __hs_exception_handler, but we do want
   to register the fact that the filter was called.  This allows us to prevent
   continuing to run when the exception was completely unhandled.
   EXCEPTION_CONTINUE_EXECUTION is returned so that the OS gives the VEH
   handlers a chance to run.  */
long WINAPI __hs_exception_filter(struct _EXCEPTION_POINTERS *exception_data)
{
    long result = EXCEPTION_CONTINUE_EXECUTION;
    if (oldTopFilter)
    {
        result = (*oldTopFilter)(exception_data);
        if (EXCEPTION_CONTINUE_SEARCH == result)
            result = EXCEPTION_CONTINUE_EXECUTION;
    }

    return result;
}

void __register_hs_exception_handler( void )
{
    if (!RtsFlags.MiscFlags.install_seh_handlers)
        return;

    // Allow the VCH handler to be registered only once.
    if (NULL == __hs_handle)
    {
        // Be the last one to run, We can then be sure we didn't interfere with
        // anything else.
        __hs_handle = AddVectoredContinueHandler(CALL_LAST,
                                                 __hs_exception_handler);
        // should the handler not be registered this will return a null.
        CHECK(__hs_handle);

        // Register for an exception filter to ensure the continue handler gets
        // hit if no one handled the exception.
        oldTopFilter = SetUnhandledExceptionFilter (__hs_exception_filter);
    }
    else
    {
        errorBelch("There is no need to call __register_hs_exception_handler()"
                   " twice, VEH handlers are global per process.");
    }
}

void __unregister_hs_exception_handler( void )
{
    if (!RtsFlags.MiscFlags.install_seh_handlers)
        return;

    if (__hs_handle != NULL)
    {
        // Should the return value be checked? we're terminating anyway.
        RemoveVectoredContinueHandler(__hs_handle);
        __hs_handle = NULL;
    }
    else
    {
        errorBelch("__unregister_hs_exception_handler() called without having"
                   "called __register_hs_exception_handler() first.");
    }
}

// Generate a crash dump, however in order for these to generate undecorated
// names we really need to be able to generate PDB files.
void generateDump (EXCEPTION_POINTERS* pExceptionPointers)
{
    if (!RtsFlags.MiscFlags.generate_dump_file)
        return;

    WCHAR szPath[MAX_PATH];
    WCHAR szFileName[MAX_PATH];
    WCHAR const *const szAppName = L"ghc";
    WCHAR const *const szVersion = L"";
    DWORD dwBufferSize = MAX_PATH;
    HANDLE hDumpFile;
    SYSTEMTIME stLocalTime;
    MINIDUMP_EXCEPTION_INFORMATION ExpParam;

    GetLocalTime (&stLocalTime);
    GetTempPathW (dwBufferSize, szPath);

    swprintf (szFileName, MAX_PATH,
              L"%ls%ls%ls-%04d%02d%02d-%02d%02d%02d-%ld-%ld.dmp",
              szPath, szAppName, szVersion,
              stLocalTime.wYear, stLocalTime.wMonth, stLocalTime.wDay,
              stLocalTime.wHour, stLocalTime.wMinute, stLocalTime.wSecond,
              GetCurrentProcessId(), GetCurrentThreadId());
    hDumpFile = CreateFileW (szFileName, GENERIC_READ|GENERIC_WRITE,
                FILE_SHARE_WRITE|FILE_SHARE_READ, 0, CREATE_ALWAYS, 0, 0);

    ExpParam.ThreadId          = GetCurrentThreadId();
    ExpParam.ExceptionPointers = pExceptionPointers;
    ExpParam.ClientPointers    = TRUE;

    MiniDumpWriteDump(GetCurrentProcess(), GetCurrentProcessId(),
                      hDumpFile, MiniDumpNormal | MiniDumpWithDataSegs |
                                 MiniDumpWithThreadInfo | MiniDumpWithCodeSegs,
                      &ExpParam, NULL, NULL);

    fprintf (stderr, "Crash dump created. Dump written to:\n\t%ls", szFileName);
}

// Generate stack trace information, we can piggy back on information we know
// about in the runtime linker to resolve symbols. So this is a good opportunity
// to make the output more useful.
void generateStack (EXCEPTION_POINTERS* pExceptionPointers)
{
    if (!RtsFlags.MiscFlags.generate_stack_trace)
        return;

    PCONTEXT context = pExceptionPointers->ContextRecord;
    STACKFRAME64 stackFrame = {0};
    DWORD machineType;

#if defined(x86_64_HOST_ARCH)
    machineType = IMAGE_FILE_MACHINE_AMD64;
    stackFrame.AddrPC.Offset = context->Rip;
    stackFrame.AddrPC.Mode = AddrModeFlat;

    stackFrame.AddrFrame.Offset = context->Rbp;
    stackFrame.AddrFrame.Mode = AddrModeFlat;

    stackFrame.AddrStack.Offset = context->Rsp;
    stackFrame.AddrStack.Mode = AddrModeFlat;
#endif
    fprintf (stderr, "\n Attempting to reconstruct a stack trace...\n\n");
    if (!SymInitialize (GetCurrentProcess (), NULL, true))
        fprintf (stderr, "  \nNOTE: Symbols could not be loaded. Addresses may"
                         " be unresolved.\n\n");

    /* Maximum amount of stack frames to show.  */
    /* Phyx: I'm not sure if I should make this configurable or not. Would a
       longer stack really be more useful? usually you only care about the top
       few.  */
    int max_frames = 35;

    fprintf (stderr, "   Frame\tCode address\n");
    DWORD64 lastBp = 0; /* Prevent loops with optimized stackframes.  */
    while (StackWalk64 (machineType, GetCurrentProcess(), GetCurrentThread(),
                        &stackFrame, context, NULL, SymFunctionTableAccess64,
                        SymGetModuleBase64, NULL) && max_frames > 0)
    {
        if (stackFrame.AddrPC.Offset == 0)
        {
            fprintf (stderr, "Null address\n");
            break;
        }
        wchar_t buffer[1024];
        uintptr_t topSp = 0;
        fprintf (stderr, " * 0x%" PRIxPTR "\t%ls\n",
                 (uintptr_t)stackFrame.AddrFrame.Offset,
                 resolveSymbolAddr ((wchar_t*)&buffer, 1024,
                                   (SymbolAddr*)(intptr_t)stackFrame.AddrPC.Offset,
                                   &topSp));
        if (lastBp >= stackFrame.AddrFrame.Offset)
        {
            fprintf (stderr, "Stack frame out of sequence...\n");
            break;
        }
        lastBp = stackFrame.AddrFrame.Offset;

        max_frames--;
        if (max_frames ==0)
        {
            fprintf (stderr, "\n   ... (maximum recursion depth reached.)\n");
        }
    }
    fprintf (stderr, "\n");
    fflush(stderr);
}
