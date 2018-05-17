/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * General utility functions used in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsUtils.h"

#include "eventlog/EventLog.h"

#if USE_LIBDW
#include <Libdw.h>
#endif

#include <stdio.h>
#include <string.h>
#include <errno.h>

#if defined(HAVE_WINDOWS_H)
#include <windows.h>
#include <fcntl.h>
#endif

/* -----------------------------------------------------------------------------
   General message generation functions

   All messages should go through here.  We can't guarantee that
   stdout/stderr will be available - e.g. in a Windows program there
   is no console for generating messages, so they have to either go to
   to the debug console, or pop up message boxes.
   -------------------------------------------------------------------------- */

// Default to the stdio implementation of these hooks.
RtsMsgFunction *fatalInternalErrorFn = rtsFatalInternalErrorFn;
RtsMsgFunction *debugMsgFn           = rtsDebugMsgFn;
RtsMsgFunction *errorMsgFn           = rtsErrorMsgFn;
RtsMsgFunction *sysErrorMsgFn        = rtsSysErrorMsgFn;

void
barf(const char*s, ...)
{
  va_list ap;
  va_start(ap,s);
  (*fatalInternalErrorFn)(s,ap);
  stg_exit(EXIT_INTERNAL_ERROR); // just in case fatalInternalErrorFn() returns
  va_end(ap);
}

void
vbarf(const char*s, va_list ap)
{
  (*fatalInternalErrorFn)(s,ap);
  stg_exit(EXIT_INTERNAL_ERROR); // just in case fatalInternalErrorFn() returns
}

void
_assertFail(const char*filename, unsigned int linenum)
{
    barf("ASSERTION FAILED: file %s, line %u\n", filename, linenum);
}

void
errorBelch(const char*s, ...)
{
  va_list ap;
  va_start(ap,s);
  (*errorMsgFn)(s,ap);
  va_end(ap);
}

void
verrorBelch(const char*s, va_list ap)
{
  (*errorMsgFn)(s,ap);
}

void
sysErrorBelch(const char*s, ...)
{
  va_list ap;
  va_start(ap,s);
  (*sysErrorMsgFn)(s,ap);
  va_end(ap);
}

void
vsysErrorBelch(const char*s, va_list ap)
{
  (*sysErrorMsgFn)(s,ap);
}

void
debugBelch(const char*s, ...)
{
  va_list ap;
  va_start(ap,s);
  (*debugMsgFn)(s,ap);
  va_end(ap);
}

void
vdebugBelch(const char*s, va_list ap)
{
  (*debugMsgFn)(s,ap);
}

/* -----------------------------------------------------------------------------
   stdio versions of the message functions
   -------------------------------------------------------------------------- */

#define BUFSIZE 512

#if defined (mingw32_HOST_OS)
static int
isGUIApp(void)
{
  PIMAGE_DOS_HEADER pDOSHeader;
  PIMAGE_NT_HEADERS pPEHeader;

  pDOSHeader = (PIMAGE_DOS_HEADER) GetModuleHandleA(NULL);
  if (pDOSHeader->e_magic != IMAGE_DOS_SIGNATURE)
    return 0;

  pPEHeader = (PIMAGE_NT_HEADERS) ((char *)pDOSHeader + pDOSHeader->e_lfanew);
  if (pPEHeader->Signature != IMAGE_NT_SIGNATURE)
    return 0;

  return (pPEHeader->OptionalHeader.Subsystem == IMAGE_SUBSYSTEM_WINDOWS_GUI);
}
#endif

void GNU_ATTRIBUTE(__noreturn__)
rtsFatalInternalErrorFn(const char *s, va_list ap)
{
#if defined(mingw32_HOST_OS)
  /* Ensure we're in text mode so newlines get encoded properly.  */
  int mode = _setmode (_fileno(stderr), _O_TEXT);
  if (isGUIApp())
  {
     char title[BUFSIZE], message[BUFSIZE];

     snprintf(title,   BUFSIZE, "%s: internal error", prog_name);
     vsnprintf(message, BUFSIZE, s, ap);

     MessageBox(NULL /* hWnd */,
                message,
                title,
                MB_OK | MB_ICONERROR | MB_TASKMODAL
               );
  }
  else
#endif
  {
     /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
     if (prog_argv != NULL && prog_name != NULL) {
       fprintf(stderr, "%s: internal error: ", prog_name);
     } else {
       fprintf(stderr, "internal error: ");
     }
     vfprintf(stderr, s, ap);
#if USE_LIBDW
     fprintf(stderr, "\n");
     fprintf(stderr, "Stack trace:\n");
     LibdwSession *session = libdwInit();
     Backtrace *bt = libdwGetBacktrace(session);
     libdwPrintBacktrace(session, stderr, bt);
     libdwFree(session);
#endif
     fprintf(stderr, "\n");
     fprintf(stderr, "    (GHC version %s for %s)\n", ProjectVersion, xstr(HostPlatform_TYPE));
     fprintf(stderr, "    Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug\n");
     fflush(stderr);
  }
#if defined(mingw32_HOST_OS)
  _setmode (_fileno(stderr), mode);
#endif

#if defined(TRACING)
  if (RtsFlags.TraceFlags.tracing == TRACE_EVENTLOG) endEventLogging();
#endif

  abort();
  // stg_exit(EXIT_INTERNAL_ERROR);
}

void
rtsErrorMsgFn(const char *s, va_list ap)
{
#if defined(mingw32_HOST_OS)
  /* Ensure we're in text mode so newlines get encoded properly.  */
  int mode = _setmode (_fileno(stderr), _O_TEXT);
  if (isGUIApp())
  {
     char buf[BUFSIZE];
     int r;

         r = vsnprintf(buf, BUFSIZE, s, ap);
         if (r > 0 && r < BUFSIZE) {
                MessageBox(NULL /* hWnd */,
              buf,
              prog_name,
              MB_OK | MB_ICONERROR | MB_TASKMODAL
              );
     }
  }
  else
#endif
  {
     /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
     if (prog_name != NULL) {
       fprintf(stderr, "%s: ", prog_name);
     }
     vfprintf(stderr, s, ap);
     fprintf(stderr, "\n");
  }
#if defined(mingw32_HOST_OS)
  _setmode (_fileno(stderr), mode);
#endif
}

void
rtsSysErrorMsgFn(const char *s, va_list ap)
{
    char *syserr;

#if defined(mingw32_HOST_OS)
    /* Ensure we're in text mode so newlines get encoded properly.  */
    int mode = _setmode (_fileno(stderr), _O_TEXT);
    FormatMessage(
        FORMAT_MESSAGE_ALLOCATE_BUFFER |
        FORMAT_MESSAGE_FROM_SYSTEM |
        FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL,
        GetLastError(),
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
        (LPTSTR) &syserr,
        0,
        NULL );

    if (isGUIApp())
    {
        char buf[BUFSIZE];
        int r;

        r = vsnprintf(buf, BUFSIZE, s, ap);
        if (r > 0 && r < BUFSIZE) {
            r = vsnprintf(buf+r, BUFSIZE-r, ": %s", syserr);
            MessageBox(NULL /* hWnd */,
                       buf,
                       prog_name,
                       MB_OK | MB_ICONERROR | MB_TASKMODAL
                );
        }
    }
    else
#else
    syserr = strerror(errno);
    // ToDo: use strerror_r() if available
#endif
    {
        /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
        if (prog_argv != NULL && prog_name != NULL) {
            fprintf(stderr, "%s: ", prog_name);
        }
        vfprintf(stderr, s, ap);
        if (syserr) {
#if defined(mingw32_HOST_OS)
            // Win32 error messages have a terminating \n
            fprintf(stderr, ": %s", syserr);
#else
            fprintf(stderr, ": %s\n", syserr);
#endif
        } else {
            fprintf(stderr, "\n");
        }
    }

#if defined(mingw32_HOST_OS)
    if (syserr) LocalFree(syserr);
    _setmode (_fileno(stderr), mode);
#endif
}

void
rtsDebugMsgFn(const char *s, va_list ap)
{
#if defined(mingw32_HOST_OS)
  /* Ensure we're in text mode so newlines get encoded properly.  */
  int mode = _setmode (_fileno(stderr), _O_TEXT);
  if (isGUIApp())
  {
     char buf[BUFSIZE];
         int r;

         r = vsnprintf(buf, BUFSIZE, s, ap);
         if (r > 0 && r < BUFSIZE) {
       OutputDebugString(buf);
     }
  }
  else
#endif
  {
     /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
     vfprintf(stderr, s, ap);
     fflush(stderr);
  }
#if defined(mingw32_HOST_OS)
  _setmode (_fileno(stderr), mode);
#endif
}


// Used in stg_badAlignment_entry defined in StgStartup.cmm.
void rtsBadAlignmentBarf(void) GNUC3_ATTRIBUTE(__noreturn__);

void
rtsBadAlignmentBarf()
{
    barf("Encountered incorrectly aligned pointer. This can't be good.");
}
