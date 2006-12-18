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
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Ticky.h"

#ifdef HAVE_TIME_H
#include <time.h>
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
                 debugBelch("Warning: %p still allocated at shutdown\n",
                            a->addr);)
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
                 debugBelch("Ignoring allocation %p %zd as allocs is NULL\n",
                            addr, len);)
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
  /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
  OutOfHeapHook(0/*unknown request size*/, 
		RtsFlags.GcFlags.maxHeapSize * BLOCK_SIZE);
  
#if defined(TICKY_TICKY)
  if (RtsFlags.TickyFlags.showTickyStats) PrintTickyInfo();
#endif

  stg_exit(EXIT_HEAPOVERFLOW);
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

static I_ __GenSymCounter = 0;

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

#if defined(PROFILING) || defined(DEBUG) || defined(PAR) || defined(GRAN)
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
#endif

/* -----------------------------------------------------------------------------
 * Reset a file handle to blocking mode.  We do this for the standard
 * file descriptors before exiting, because the shell doesn't always
 * clean up for us.
 * -------------------------------------------------------------------------- */

#if !defined(mingw32_HOST_OS)
void
resetNonBlockingFd(int fd)
{
  long fd_flags;

  /* clear the non-blocking flag on this file descriptor */
  fd_flags = fcntl(fd, F_GETFL);
  if (fd_flags & O_NONBLOCK) {
    fcntl(fd, F_SETFL, fd_flags & ~O_NONBLOCK);
  }
}

void
setNonBlockingFd(int fd)
{
  long fd_flags;

  /* clear the non-blocking flag on this file descriptor */
  fd_flags = fcntl(fd, F_GETFL);
  if (!(fd_flags & O_NONBLOCK)) {
    fcntl(fd, F_SETFL, fd_flags | O_NONBLOCK);
  }
}
#else
/* Stub defns -- async / non-blocking IO is not done 
 * via O_NONBLOCK and select() under Win32. 
 */
void resetNonBlockingFd(int fd STG_UNUSED) {}
void setNonBlockingFd(int fd STG_UNUSED) {}
#endif

#ifdef PAR
static ullong startTime = 0;

/* used in a parallel setup */
ullong
msTime(void)
{
# if defined(HAVE_GETCLOCK) && !defined(alpha_HOST_ARCH) && !defined(hppa1_1_HOST_ARCH)
    struct timespec tv;

    if (getclock(TIMEOFDAY, &tv) != 0) {
	fflush(stdout);
	fprintf(stderr, "Clock failed\n");
	stg_exit(EXIT_FAILURE);
    }
    return tv.tv_sec * LL(1000) + tv.tv_nsec / LL(1000000) - startTime;
# elif HAVE_GETTIMEOFDAY && !defined(alpha_HOST_ARCH)
    struct timeval tv;
 
    if (gettimeofday(&tv, NULL) != 0) {
	fflush(stdout);
	fprintf(stderr, "Clock failed\n");
	stg_exit(EXIT_FAILURE);
    }
    return tv.tv_sec * LL(1000) + tv.tv_usec / LL(1000) - startTime;
# else
    time_t t;
    if ((t = time(NULL)) == (time_t) -1) {
	fflush(stdout);
	fprintf(stderr, "Clock failed\n");
	stg_exit(EXIT_FAILURE);
    }
    return t * LL(1000) - startTime;
# endif
}
#endif /* PAR */

/* -----------------------------------------------------------------------------
   Print large numbers, with punctuation.
   -------------------------------------------------------------------------- */

char *
ullong_format_string(ullong x, char *s, rtsBool with_commas)
{
    if (x < (ullong)1000) 
	sprintf(s, "%lu", (lnat)x);
    else if (x < (ullong)1000000)
	sprintf(s, (with_commas) ? "%lu,%3.3lu" : "%lu%3.3lu",
		(lnat)((x)/(ullong)1000),
		(lnat)((x)%(ullong)1000));
    else if (x < (ullong)1000000000)
	sprintf(s, (with_commas) ? "%lu,%3.3lu,%3.3lu" :  "%lu%3.3lu%3.3lu",
		(lnat)((x)/(ullong)1000000),
		(lnat)((x)/(ullong)1000%(ullong)1000),
		(lnat)((x)%(ullong)1000));
    else
	sprintf(s, (with_commas) ? "%lu,%3.3lu,%3.3lu,%3.3lu" : "%lu%3.3lu%3.3lu%3.3lu",
		(lnat)((x)/(ullong)1000000000),
		(lnat)((x)/(ullong)1000000%(ullong)1000),
		(lnat)((x)/(ullong)1000%(ullong)1000), 
		(lnat)((x)%(ullong)1000));
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
#if defined(THREADED_RTS) && (defined(openbsd_HOST_OS) || defined(freebsd_HOST_OS))
        return pthread_kill(pthread_self(), sig);
#else
        return raise(sig);
#endif
}
