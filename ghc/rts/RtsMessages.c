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

/* -----------------------------------------------------------------------------
   General message generation functions

   All messages should go through here.  We can't guarantee that
   stdout/stderr will be available - e.g. in a Windows program there
   is no console for generating messages, so they have to either go to
   to the debug console, or pop up message boxes.
   -------------------------------------------------------------------------- */

// Default to the stdio implementation of these hooks.
RtsMsgFunction *fatalInternalErrorFn = stdioFatalInternalErrorFn;
RtsMsgFunction *debugMsgFn           = stdioDebugMsgFn;
RtsMsgFunction *errorMsgFn           = stdioErrorMsgFn;

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

void 
stdioFatalInternalErrorFn(char *s, va_list ap)
{
  /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
  if (prog_argv != NULL && prog_name != NULL) {
    fprintf(stderr, "%s: internal error: ", prog_name);
  } else {
    fprintf(stderr, "internal error: ");
  }
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
  fprintf(stderr, "    Please report this as a bug to glasgow-haskell-bugs@haskell.org,\n    or http://www.sourceforge.net/projects/ghc/\n");
  fflush(stderr);
  stg_exit(EXIT_INTERNAL_ERROR);
}

void
stdioErrorMsgFn(char *s, va_list ap)
{
  /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
  if (prog_argv != NULL && prog_name != NULL) {
    fprintf(stderr, "%s: ", prog_name);
  } 
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
}

void
stdioDebugMsgFn(char *s, va_list ap)
{
  /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
  vfprintf(stderr, s, ap);
  fflush(stderr);
}

