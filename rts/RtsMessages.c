/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * General utility functions used in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include <stdio.h>

#ifdef HAVE_WINDOWS_H
#include <windows.h>
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

void
barf(char *s, ...)
{
  va_list ap;
  va_start(ap,s);
  (*fatalInternalErrorFn)(s,ap);
  stg_exit(EXIT_INTERNAL_ERROR); // just in case fatalInternalErrorFn() returns
  va_end(ap);
}

void
vbarf(char *s, va_list ap)
{
  (*fatalInternalErrorFn)(s,ap);
  stg_exit(EXIT_INTERNAL_ERROR); // just in case fatalInternalErrorFn() returns
}

void 
_assertFail(char *filename, unsigned int linenum)
{
    barf("ASSERTION FAILED: file %s, line %u\n", filename, linenum);
}

void
errorBelch(char *s, ...)
{
  va_list ap;
  va_start(ap,s);
  (*errorMsgFn)(s,ap);
  va_end(ap);
}

void
verrorBelch(char *s, va_list ap)
{
  (*errorMsgFn)(s,ap);
}

void
debugBelch(char *s, ...)
{
  va_list ap;
  va_start(ap,s);
  (*debugMsgFn)(s,ap);
  va_end(ap);
}

void
vdebugBelch(char *s, va_list ap)
{
  (*debugMsgFn)(s,ap);
}

/* -----------------------------------------------------------------------------
   stdio versions of the message functions
   -------------------------------------------------------------------------- */

#define BUFSIZE 512

#if defined(cygwin32_HOST_OS) || defined (mingw32_HOST_OS)
static int
isGUIApp()
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

#define xstr(s) str(s)
#define str(s) #s

void
rtsFatalInternalErrorFn(char *s, va_list ap)
{
#if defined(cygwin32_HOST_OS) || defined (mingw32_HOST_OS)
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
     fprintf(stderr, "\n");
     fprintf(stderr, "    (GHC version %s for %s)\n", ProjectVersion, xstr(HostPlatform_TYPE));
     fprintf(stderr, "    Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug\n");
     fflush(stderr);
  }

  abort();
  // stg_exit(EXIT_INTERNAL_ERROR);
}

void
rtsErrorMsgFn(char *s, va_list ap)
{
#if defined(cygwin32_HOST_OS) || defined (mingw32_HOST_OS)
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
     if (prog_argv != NULL && prog_name != NULL) {
       fprintf(stderr, "%s: ", prog_name);
     }
     vfprintf(stderr, s, ap);
     fprintf(stderr, "\n");
  }
}

void
rtsDebugMsgFn(char *s, va_list ap)
{
#if defined(cygwin32_HOST_OS) || defined (mingw32_HOST_OS)
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
}
