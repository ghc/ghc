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
   Debugging allocator
   -------------------------------------------------------------------------- */

#if defined(DEBUG)

typedef struct Allocated_ {
    void *addr;
    size_t len;
    struct Allocated_ *next;
} Allocated;

static Allocated *allocs = NULL;

#ifdef THREADED_RTS
static Mutex allocator_mutex;
#endif

void
initAllocator(void)
{
    Allocated *a;
    size_t alloc_size;

#ifdef THREADED_RTS
    initMutex(&allocator_mutex);
#endif
    alloc_size = sizeof(Allocated);
    if ((a = (Allocated *) malloc(alloc_size)) == NULL) {
      /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
      MallocFailHook((W_) alloc_size, "initialising debugging allocator");
      stg_exit(EXIT_INTERNAL_ERROR);
    }
    a->addr = NULL;
    a->len = 0;
    a->next = NULL;
    allocs = a;
}

void
shutdownAllocator(void)
{
    Allocated *prev, *a;

    if (allocs == NULL) {
        barf("Allocator shutdown requested, but not initialised!");
    }

#ifdef THREADED_RTS
    closeMutex(&allocator_mutex);
#endif

    prev = allocs;
    while (1) {
        a = prev->next;
        free(prev);
        if (a == NULL) return;
        IF_DEBUG(sanity,
                 debugBelch("Warning: %ld bytes at %p still allocated at shutdown\n",
                            (long)a->len, a->addr);)
        prev = a;
    }
}

static void addAllocation(void *addr, size_t len) {
    Allocated *a;
    size_t alloc_size;

    if (allocs != NULL) {
        alloc_size = sizeof(Allocated);
        if ((a = (Allocated *) malloc(alloc_size)) == NULL) {
          /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
          MallocFailHook((W_) alloc_size,
                         "creating info for debugging allocator");
          stg_exit(EXIT_INTERNAL_ERROR);
        }
        a->addr = addr;
        a->len = len;
        ACQUIRE_LOCK(&allocator_mutex);
        a->next = allocs->next;
        allocs->next = a;
        RELEASE_LOCK(&allocator_mutex);
    }
    else {
        /* This doesn't actually help as we haven't looked at the flags
         * at the time that it matters (while running constructors) */
        IF_DEBUG(sanity,
                 debugBelch("Ignoring allocation %p %d as allocs is NULL\n",
                            addr, (int)len);)
    }
}

static void removeAllocation(void *addr, int overwrite_with_aa) {
    Allocated *prev, *a;

    if (addr == NULL) {
        barf("Freeing NULL!");
    }

    if (allocs != NULL) {
        ACQUIRE_LOCK(&allocator_mutex);
        prev = allocs;
        a = prev->next;
        while (a != NULL) {
            if (a->addr == addr) {
                prev->next = a->next;
                if (overwrite_with_aa) {
                    memset(addr, 0xaa, a->len);
                }
                free(a);
                RELEASE_LOCK(&allocator_mutex);
                return;
            }
            prev = a;
            a = a->next;
        }
        /* We would like to barf here, but we can't as conc021
         * allocates some stuff in a constructor which then gets freed
         * during hs_exit */
        /* barf("Freeing non-allocated memory at %p", addr); */
        IF_DEBUG(sanity,
                 debugBelch("Warning: Freeing non-allocated memory at %p\n",
                            addr);)
        RELEASE_LOCK(&allocator_mutex);
    }
    else {
        IF_DEBUG(sanity,
                 debugBelch("Ignoring free of %p as allocs is NULL\n",
                            addr);)
    }
}
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
#if defined(DEBUG)
    addAllocation(space, n2);
#endif
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
#if defined(DEBUG)
    removeAllocation(p, 0);
    addAllocation(space, n2);
#endif
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
#if defined(DEBUG)
    addAllocation(space, (size_t) n * (size_t) m);
#endif
    return space;
}

/* To simplify changing the underlying allocator used
 * by stgMallocBytes(), provide stgFree() as well.
 */
void
stgFree(void* p)
{
#if defined(DEBUG)
  removeAllocation(p, 1);
#endif
  free(p);
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
    if (!heap_overflow)
    {
        /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
        OutOfHeapHook(0/*unknown request size*/,
                      RtsFlags.GcFlags.maxHeapSize * BLOCK_SIZE);

        heap_overflow = rtsTrue;
    }
}

/* -----------------------------------------------------------------------------
   genSym stuff, used by GHC itself for its splitting unique supply.

   ToDo: put this somewhere sensible.
   -------------------------------------------------------------------------  */

static HsInt __GenSymCounter = 0;

HsInt
genSymZh(void)
{
    return(__GenSymCounter++);
}
HsInt
resetGenSymZh(void) /* it's your funeral */
{
    __GenSymCounter=0;
    return(__GenSymCounter);
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
#if defined(THREADED_RTS) && (defined(openbsd_HOST_OS) || defined(freebsd_HOST_OS) || defined(dragonfly_HOST_OS))
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
