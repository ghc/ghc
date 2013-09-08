/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * General utility functions used in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsAPI.h"

#include "RtsUtils.h"
#include "Ticky.h"
#include "Schedule.h"

#ifdef HAVE_TIME_H
#include <time.h>
#endif

/* HACK: On Mac OS X 10.4 (at least), time.h doesn't declare ctime_r with
 *       _POSIX_C_SOURCE. If this is the case, we declare it ourselves.
 */
#if HAVE_CTIME_R && !HAVE_DECL_CTIME_R
extern char *ctime_r(const time_t *, char *);
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_GETTIMEOFDAY
#include <sys/time.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#if defined(THREADED_RTS) && defined(openbsd_HOST_OS) && defined(HAVE_PTHREAD_H)
#include <pthread.h>
#endif


#if defined(_WIN32)
#include <windows.h>
#endif

/* -----------------------------------------------------------------------------
   Result-checking malloc wrappers.
   -------------------------------------------------------------------------- */

void *
stgMallocBytes (int n, char *msg)
{
    char *space;
    size_t n2;

    n2 = (size_t) n;
    if ((space = (char *) malloc(n2)) == NULL) {
      /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
      MallocFailHook((W_) n, msg); /*msg*/
      stg_exit(EXIT_INTERNAL_ERROR);
    }
    return space;
}

void *
stgReallocBytes (void *p, int n, char *msg)
{
    char *space;
    size_t n2;

    n2 = (size_t) n;
    if ((space = (char *) realloc(p, (size_t) n2)) == NULL) {
      /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
      MallocFailHook((W_) n, msg); /*msg*/
      stg_exit(EXIT_INTERNAL_ERROR);
    }
    return space;
}

void *
stgCallocBytes (int n, int m, char *msg)
{
    char *space;

    if ((space = (char *) calloc((size_t) n, (size_t) m)) == NULL) {
      /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
      MallocFailHook((W_) n*m, msg); /*msg*/
      stg_exit(EXIT_INTERNAL_ERROR);
    }
    return space;
}

/* To simplify changing the underlying allocator used
 * by stgMallocBytes(), provide stgFree() as well.
 */
void
stgFree(void* p)
{
  free(p);
}

/* -----------------------------------------------------------------------------
   Stack overflow
   
   Not sure if this belongs here.
   -------------------------------------------------------------------------- */

void
stackOverflow(StgTSO* tso)
{
    StackOverflowHook(tso->tot_stack_size * sizeof(W_));

#if defined(TICKY_TICKY)
    if (RtsFlags.TickyFlags.showTickyStats) PrintTickyInfo();
#endif
}

void
heapOverflow(void)
{
    if (!heap_overflow)
    {
        /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
        OutOfHeapHook(0/*unknown request size*/,
                      (W_)RtsFlags.GcFlags.maxHeapSize * BLOCK_SIZE);

        heap_overflow = rtsTrue;
    }
}

/* -----------------------------------------------------------------------------
   Get the current time as a string.  Used in profiling reports.
   -------------------------------------------------------------------------- */

char *
time_str(void)
{
    static time_t now = 0;
    static char nowstr[26];

    if (now == 0) {
	time(&now);
#if HAVE_CTIME_R
	ctime_r(&now, nowstr);
#else
	strcpy(nowstr, ctime(&now));
#endif
	memmove(nowstr+16,nowstr+19,7);
	nowstr[21] = '\0';  // removes the \n
    }
    return nowstr;
}

/* -----------------------------------------------------------------------------
   Print large numbers, with punctuation.
   -------------------------------------------------------------------------- */

char *
showStgWord64(StgWord64 x, char *s, rtsBool with_commas)
{
    if (with_commas) {
        if (x < (StgWord64)1e3)
                sprintf(s, "%" FMT_Word64, (StgWord64)x);
        else if (x < (StgWord64)1e6)
                sprintf(s, "%" FMT_Word64 ",%03" FMT_Word64,
                        (StgWord64)(x / 1000),
                        (StgWord64)(x % 1000));
        else if (x < (StgWord64)1e9)
                sprintf(s, "%"    FMT_Word64
                           ",%03" FMT_Word64
                           ",%03" FMT_Word64,
                        (StgWord64)(x / 1e6),
                        (StgWord64)((x / 1000) % 1000),
                        (StgWord64)(x          % 1000));
        else if (x < (StgWord64)1e12)
                sprintf(s, "%"    FMT_Word64
                           ",%03" FMT_Word64
                           ",%03" FMT_Word64
                           ",%03" FMT_Word64,
                        (StgWord64)(x / (StgWord64)1e9),
                        (StgWord64)((x / (StgWord64)1e6) % 1000),
                        (StgWord64)((x / (StgWord64)1e3) % 1000),
                        (StgWord64)(x                    % 1000));
        else if (x < (StgWord64)1e15)
                sprintf(s, "%"    FMT_Word64
                           ",%03" FMT_Word64
                           ",%03" FMT_Word64
                           ",%03" FMT_Word64
                           ",%03" FMT_Word64,
                        (StgWord64)(x / (StgWord64)1e12),
                        (StgWord64)((x / (StgWord64)1e9) % 1000),
                        (StgWord64)((x / (StgWord64)1e6) % 1000),
                        (StgWord64)((x / (StgWord64)1e3) % 1000),
                        (StgWord64)(x                    % 1000));
        else if (x < (StgWord64)1e18)
                sprintf(s, "%"    FMT_Word64
                           ",%03" FMT_Word64
                           ",%03" FMT_Word64
                           ",%03" FMT_Word64
                           ",%03" FMT_Word64
                           ",%03" FMT_Word64,
                        (StgWord64)(x / (StgWord64)1e15),
                        (StgWord64)((x / (StgWord64)1e12) % 1000),
                        (StgWord64)((x / (StgWord64)1e9)  % 1000),
                        (StgWord64)((x / (StgWord64)1e6)  % 1000),
                        (StgWord64)((x / (StgWord64)1e3)  % 1000),
                        (StgWord64)(x                     % 1000));
        else
                sprintf(s, "%"    FMT_Word64
                           ",%03" FMT_Word64
                           ",%03" FMT_Word64
                           ",%03" FMT_Word64
                           ",%03" FMT_Word64
                           ",%03" FMT_Word64
                           ",%03" FMT_Word64,
                        (StgWord64)(x / (StgWord64)1e18),
                        (StgWord64)((x / (StgWord64)1e15) % 1000),
                        (StgWord64)((x / (StgWord64)1e12) % 1000),
                        (StgWord64)((x / (StgWord64)1e9)  % 1000),
                        (StgWord64)((x / (StgWord64)1e6)  % 1000),
                        (StgWord64)((x / (StgWord64)1e3)  % 1000),
                        (StgWord64)(x                     % 1000));
    }
    else {
        sprintf(s, "%" FMT_Word64, x);
    }
    return s;
}


// Can be used as a breakpoint to set on every heap check failure.
#ifdef DEBUG
void
heapCheckFail( void )
{
}
#endif

/* 
 * It seems that pthreads and signals interact oddly in OpenBSD & FreeBSD
 * pthreads (and possibly others). When linking with -lpthreads, we
 * have to use pthread_kill to send blockable signals. So use that
 * when we have a threaded rts. So System.Posix.Signals will call
 * genericRaise(), rather than raise(3).
 */
int genericRaise(int sig) {
#if defined(THREADED_RTS) && (defined(openbsd_HOST_OS) || defined(freebsd_HOST_OS) || defined(dragonfly_HOST_OS) || defined(netbsd_HOST_OS) || defined(darwin_HOST_OS))
        return pthread_kill(pthread_self(), sig);
#else
        return raise(sig);
#endif
}

static void mkRtsInfoPair(char *key, char *val) {
    /* XXX should check for "s, \s etc in key and val */
    printf(" ,(\"%s\", \"%s\")\n", key, val);
}

/* This little bit of magic allows us to say TOSTRING(SYM) and get
 * "5" if SYM is 5 */
#define TOSTRING2(x) #x
#define TOSTRING(x)  TOSTRING2(x)

void printRtsInfo(void) {
    /* The first entry is just a hack to make it easy to get the
     * commas right */
    printf(" [(\"GHC RTS\", \"YES\")\n");
    mkRtsInfoPair("GHC version",             ProjectVersion);
    mkRtsInfoPair("RTS way",                 RtsWay);
    mkRtsInfoPair("Build platform",          BuildPlatform);
    mkRtsInfoPair("Build architecture",      BuildArch);
    mkRtsInfoPair("Build OS",                BuildOS);
    mkRtsInfoPair("Build vendor",            BuildVendor);
    mkRtsInfoPair("Host platform",           HostPlatform);
    mkRtsInfoPair("Host architecture",       HostArch);
    mkRtsInfoPair("Host OS",                 HostOS);
    mkRtsInfoPair("Host vendor",             HostVendor);
    mkRtsInfoPair("Target platform",         TargetPlatform);
    mkRtsInfoPair("Target architecture",     TargetArch);
    mkRtsInfoPair("Target OS",               TargetOS);
    mkRtsInfoPair("Target vendor",           TargetVendor);
    mkRtsInfoPair("Word size",               TOSTRING(WORD_SIZE_IN_BITS));
    mkRtsInfoPair("Compiler unregisterised", GhcUnregisterised);
    mkRtsInfoPair("Tables next to code",     GhcEnableTablesNextToCode);
    printf(" ]\n");
}

// Provides a way for Haskell programs to tell whether they're being
// profiled or not.  GHCi uses it (see #2197).
int rts_isProfiled(void)
{
#ifdef PROFILING
    return 1;
#else
    return 0;
#endif
}

// Used for detecting a non-empty FPU stack on x86 (see #4914)
void checkFPUStack(void)
{
#ifdef x86_HOST_ARCH
    static unsigned char buf[108];
    asm("FSAVE %0":"=m" (buf));

    if(buf[8]!=255 || buf[9]!=255) {
        errorBelch("NONEMPTY FPU Stack, TAG = %x %x\n",buf[8],buf[9]);
        abort();
    }
#endif
}

