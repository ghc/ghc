/* -----------------------------------------------------------------------------
 * $Id: RtsUtils.c,v 1.4 1999/01/27 14:51:21 simonpj Exp $
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

#include <stdarg.h>

/* variable-argument error function. */

void barf(char *s, ...)
{
  va_list ap;
  va_start(ap,s);
  fflush(stdout);
  if (prog_argv != NULL && prog_argv[0] != NULL) {
    fprintf(stderr, "%s: fatal error: ", prog_argv[0]);
  } else {
    fprintf(stderr, "fatal error: ");
  }
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
  stg_exit(EXIT_FAILURE);
}

void belch(char *s, ...)
{
  va_list ap;
  va_start(ap,s);
  fflush(stdout);
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
}

/* result-checking malloc wrappers. */

void *
stgMallocBytes (int n, char *msg)
{
    char *space;

    if ((space = (char *) malloc((size_t) n)) == NULL) {
	fflush(stdout);
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
	fflush(stdout);
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
  fflush(stdout);
  fprintf(stderr, "ASSERTION FAILED: file %s, line %u\n", filename, linenum);
  abort();
}

StgStablePtr errorHandler = -1; /* -1 indicates no handler installed */

void
raiseError( StgStablePtr handler STG_UNUSED )
{
  shutdownHaskell();
  stg_exit(EXIT_FAILURE);
}

/* -----------------------------------------------------------------------------
   Stack overflow
   
   Not sure if this belongs here.
   -------------------------------------------------------------------------- */

void
stackOverflow(nat max_stack_size)
{
    fflush(stdout);
    StackOverflowHook(max_stack_size * sizeof(W_)); /*msg*/

#if defined(TICKY_TICKY)
    if (RtsFlags.TickyFlags.showTickyStats) PrintTickyInfo();
#endif

    stg_exit(EXIT_FAILURE);
}

void
heapOverflow(void)
{
    fflush(stdout);
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
genSymzh(void)
{
    return(__GenSymCounter++);
}
I_
resetGenSymzh(void) /* it's your funeral */
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
