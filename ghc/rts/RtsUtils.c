/* -----------------------------------------------------------------------------
 * $Id: RtsUtils.c,v 1.11 2000/01/12 15:15:17 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * General utility functions used in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsAPI.h"
#include "RtsFlags.h"
#include "Hooks.h"
#include "Main.h"
#include "RtsUtils.h"
#include "Ticky.h"

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <stdarg.h>

/* variable-argument error function. */

void barf(char *s, ...)
{
  va_list ap;
  va_start(ap,s);
  /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
  if (prog_argv != NULL && prog_argv[0] != NULL) {
    fprintf(stderr, "%s: fatal error: ", prog_argv[0]);
  } else {
    fprintf(stderr, "fatal error: ");
  }
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
  fflush(stderr);
  stg_exit(EXIT_FAILURE);
}

void prog_belch(char *s, ...)
{
  va_list ap;
  va_start(ap,s);
  /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
  if (prog_argv != NULL && prog_argv[0] != NULL) {
    fprintf(stderr, "%s: ", prog_argv[0]);
  } 
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
}

void belch(char *s, ...)
{
  va_list ap;
  va_start(ap,s);
  /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
}

/* result-checking malloc wrappers. */

void *
stgMallocBytes (int n, char *msg)
{
    char *space;

    if ((space = (char *) malloc((size_t) n)) == NULL) {
      /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
	MallocFailHook((W_) n, msg); /*msg*/
	stg_exit(EXIT_FAILURE);
    }
    return space;
}

void *
stgReallocBytes (void *p, int n, char *msg)
{
    char *space;

    if ((space = (char *) realloc(p, (size_t) n)) == NULL) {
      /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
	MallocFailHook((W_) n, msg); /*msg*/
	exit(EXIT_FAILURE);
    }
    return space;
}

void *
stgMallocWords (int n, char *msg)
{
  return(stgMallocBytes(n * sizeof(W_), msg));
}

void *
stgReallocWords (void *p, int n, char *msg)
{
  return(stgReallocBytes(p, n * sizeof(W_), msg));
}

void 
_stgAssert (char *filename, nat linenum)
{
  /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
  fprintf(stderr, "ASSERTION FAILED: file %s, line %u\n", filename, linenum);
  abort();
}

/* -----------------------------------------------------------------------------
   Stack overflow
   
   Not sure if this belongs here.
   -------------------------------------------------------------------------- */

void
stackOverflow(void)
{
  StackOverflowHook(RtsFlags.GcFlags.maxStkSize * sizeof(W_));

#if defined(TICKY_TICKY)
  if (RtsFlags.TickyFlags.showTickyStats) PrintTickyInfo();
#endif
}

void
heapOverflow(void)
{
  /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
  OutOfHeapHook(0/*unknown request size*/, 
		RtsFlags.GcFlags.maxHeapSize * BLOCK_SIZE);
  
#if defined(TICKY_TICKY)
  if (RtsFlags.TickyFlags.showTickyStats) PrintTickyInfo();
#endif

  stg_exit(EXIT_FAILURE);
}

/* -----------------------------------------------------------------------------
   Out-of-line strlen.

   Used in addr2Integer because the C compiler on x86 chokes on
   strlen, trying to inline it with not enough registers available.
   -------------------------------------------------------------------------- */

nat stg_strlen(char *s)
{
   char *p = s;

   while (*p) p++;
   return p-s;
}


/* -----------------------------------------------------------------------------
   genSym stuff, used by GHC itself for its splitting unique supply.

   ToDo: put this somewhere sensible.
   -------------------------------------------------------------------------  */

I_ __GenSymCounter = 0;

I_
genSymZh(void)
{
    return(__GenSymCounter++);
}
I_
resetGenSymZh(void) /* it's your funeral */
{
    __GenSymCounter=0;
    return(__GenSymCounter);
}

/* -----------------------------------------------------------------------------
   Get the current time as a string.  Used in profiling reports.
   -------------------------------------------------------------------------- */

#if defined(PROFILING) || defined(DEBUG)
char *
time_str(void)
{
    static time_t now = 0;
    static char nowstr[26];

    if (now == 0) {
	time(&now);
	strcpy(nowstr, ctime(&now));
	strcpy(nowstr+16,nowstr+19);
	nowstr[21] = '\0';
    }
    return nowstr;
}
#endif

/* -----------------------------------------------------------------------------
 * Reset a file handle to blocking mode.  We do this for the standard
 * file descriptors before exiting, because the shell doesn't always
 * clean up for us.
 * -------------------------------------------------------------------------- */

void
resetNonBlockingFd(int fd)
{
  long fd_flags;

#if !defined(_WIN32) || defined(__CYGWIN__) || defined(__CYGWIN32__)
  /* clear the non-blocking flag on this file descriptor */
  fd_flags = fcntl(fd, F_GETFL);
  if (fd_flags & O_NONBLOCK) {
    fcntl(fd, F_SETFL, fd_flags & ~O_NONBLOCK);
  }
#endif
}

/* -----------------------------------------------------------------------------
   Print large numbers, with punctuation.
   -------------------------------------------------------------------------- */

char *
ullong_format_string(ullong x, char *s, rtsBool with_commas)
{
    if (x < (ullong)1000) 
	sprintf(s, "%d", (nat)x);
    else if (x < (ullong)1000000)
	sprintf(s, (with_commas) ? "%ld,%3.3ld" : "%ld%3.3ld",
		(nat)((x)/(ullong)1000),
		(nat)((x)%(ullong)1000));
    else if (x < (ullong)1000000000)
	sprintf(s, (with_commas) ? "%ld,%3.3ld,%3.3ld" :  "%ld%3.3ld%3.3ld",
		(nat)((x)/(ullong)1000000),
		(nat)((x)/(ullong)1000%(ullong)1000),
		(nat)((x)%(ullong)1000));
    else
	sprintf(s, (with_commas) ? "%ld,%3.3ld,%3.3ld,%3.3ld" : "%ld%3.3ld%3.3ld%3.3ld",
		(nat)((x)/(ullong)1000000000),
		(nat)((x)/(ullong)1000000%(ullong)1000),
		(nat)((x)/(ullong)1000%(ullong)1000), 
		(nat)((x)%(ullong)1000));
    return s;
}
