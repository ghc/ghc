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

#if defined(openbsd_HOST_OS) || defined(linux_HOST_OS) || defined(darwin_HOST_OS)
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>

/* no C99 header stdint.h on OpenBSD? */
#if defined(openbsd_HOST_OS)
typedef unsigned long my_uintptr_t;
#else
#include <stdint.h>
typedef uintptr_t my_uintptr_t;
#endif
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

    if ((space = (char *) malloc((size_t) n)) == NULL) {
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

    if ((space = (char *) realloc(p, (size_t) n)) == NULL) {
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

/* -----------------------------------------------------------------------------
   Allocating executable memory
   -------------------------------------------------------------------------- */

/* Heavily arch-specific, I'm afraid.. */

/*
 * Allocate len bytes which are readable, writable, and executable.
 *
 * ToDo: If this turns out to be a performance bottleneck, one could
 * e.g. cache the last VirtualProtect/mprotect-ed region and do
 * nothing in case of a cache hit.
 */
void*
stgMallocBytesRWX(int len)
{
  void *addr = stgMallocBytes(len, "mallocBytesRWX");
#if defined(i386_HOST_ARCH) && defined(_WIN32)
  /* This could be necessary for processors which distinguish between READ and
     EXECUTE memory accesses, e.g. Itaniums. */
  DWORD dwOldProtect = 0;
  if (VirtualProtect (addr, len, PAGE_EXECUTE_READWRITE, &dwOldProtect) == 0) {
    barf("mallocBytesRWX: failed to protect 0x%p; error=%lu; old protection: %lu\n",
         addr, (unsigned long)GetLastError(), (unsigned long)dwOldProtect);
  }
#elif defined(openbsd_HOST_OS) || defined(linux_HOST_OS) || defined(darwin_HOST_OS)
  /* malloced memory isn't executable by default on OpenBSD */
  my_uintptr_t pageSize         = sysconf(_SC_PAGESIZE);
  my_uintptr_t mask             = ~(pageSize - 1);
  my_uintptr_t startOfFirstPage = ((my_uintptr_t)addr          ) & mask;
  my_uintptr_t startOfLastPage  = ((my_uintptr_t)addr + len - 1) & mask;
  my_uintptr_t size             = startOfLastPage - startOfFirstPage + pageSize;
  if (mprotect((void*)startOfFirstPage, (size_t)size, PROT_EXEC | PROT_READ | PROT_WRITE) != 0) {
    barf("mallocBytesRWX: failed to protect 0x%p\n", addr);
  }
#endif
  return addr;
}
