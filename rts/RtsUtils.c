/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * General utility functions used in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"
#include "RtsAPI.h"

#include "RtsUtils.h"
#include "Ticky.h"
#include "Schedule.h"
#include "RtsFlags.h"
#include "IOManager.h"

#include <time.h>

/* HACK: On Mac OS X 10.4 (at least), time.h doesn't declare ctime_r with
 *       _POSIX_C_SOURCE. If this is the case, we declare it ourselves.
 */
#if defined(HAVE_CTIME_R) && !HAVE_DECL_CTIME_R
extern char *ctime_r(const time_t *, char *);
#endif

#if defined(HAVE_FCNTL_H)
#include <fcntl.h>
#endif

#if defined(HAVE_GETTIMEOFDAY)
#include <sys/time.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

#if defined(HAVE_SIGNAL_H)
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
stgMallocBytes (size_t n, char *msg)
{
    void *space = malloc(n);

    if (space == NULL) {
      /* Quoting POSIX.1-2008 (which says more or less the same as ISO C99):
       *
       *   "Upon successful completion with size not equal to 0, malloc() shall
       *   return a pointer to the allocated space. If size is 0, either a null
       *   pointer or a unique pointer that can be successfully passed to free()
       *   shall be returned. Otherwise, it shall return a null pointer and set
       *   errno to indicate the error."
       *
       * Consequently, a NULL pointer being returned by `malloc()` for a 0-size
       * allocation is *not* to be considered an error.
       */
      if (n == 0) return NULL;

      /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
      rtsConfig.mallocFailHook((W_) n, msg);
      stg_exit(EXIT_INTERNAL_ERROR);
    }
    IF_DEBUG(zero_on_gc, memset(space, 0xbb, n));
    return space;
}

void *
stgReallocBytes (void *p, size_t n, char *msg)
{
    void *space;

    if ((space = realloc(p, n)) == NULL) {
      /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
      rtsConfig.mallocFailHook((W_) n, msg);
      stg_exit(EXIT_INTERNAL_ERROR);
    }
    return space;
}

void *
stgCallocBytes (size_t count, size_t size, char *msg)
{
    void *space;

    if ((space = calloc(count, size)) == NULL) {
      /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
      rtsConfig.mallocFailHook((W_) count*size, msg);
      stg_exit(EXIT_INTERNAL_ERROR);
    }
    return space;
}

/* borrowed from the MUSL libc project */
char *stgStrndup(const char *s, size_t n)
{
    size_t l = strnlen(s, n);
    char *d = stgMallocBytes(l+1, "stgStrndup");
    if (!d) return NULL;
    memcpy(d, s, l);
    d[l] = 0;
    return d;
}


/* To simplify changing the underlying allocator used
 * by stgMallocBytes(), provide stgFree() as well.
 */
void
stgFree(void* p)
{
  free(p);
}

// N.B. Allocations resulting from this function must be freed by
// `stgFreeAligned`, not `stgFree`. This is necessary due to the properties of Windows' `_aligned_malloc`
void *
stgMallocAlignedBytes (size_t n, size_t align, char *msg)
{
    void *space;

#if defined(mingw32_HOST_OS)
    space = _aligned_malloc(n, align);
#else
    if (posix_memalign(&space, align, n)) {
        space = NULL; // Allocation failed
    }
#endif

    if (space == NULL) {
      /* Quoting POSIX.1-2008 (which says more or less the same as ISO C99):
       *
       *   "Upon successful completion with size not equal to 0, malloc() shall
       *   return a pointer to the allocated space. If size is 0, either a null
       *   pointer or a unique pointer that can be successfully passed to free()
       *   shall be returned. Otherwise, it shall return a null pointer and set
       *   errno to indicate the error."
       *
       * Consequently, a NULL pointer being returned by `malloc()` for a 0-size
       * allocation is *not* to be considered an error.
       */
      if (n == 0) return NULL;

      /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
      rtsConfig.mallocFailHook((W_) n, msg);
      stg_exit(EXIT_INTERNAL_ERROR);
    }
    IF_DEBUG(zero_on_gc, memset(space, 0xbb, n));
    return space;
}

void
stgFreeAligned (void *p)
{
#if defined(mingw32_HOST_OS)
    _aligned_free(p);
#else
    free(p);
#endif
}

/* -----------------------------------------------------------------------------
   Stack/heap overflow
   -------------------------------------------------------------------------- */

void
reportStackOverflow(StgTSO* tso)
{
    rtsConfig.stackOverflowHook(tso->tot_stack_size * sizeof(W_));

#if defined(TICKY_TICKY)
    if (RtsFlags.TickyFlags.showTickyStats) PrintTickyInfo();
#endif
}

void
reportHeapOverflow(void)
{
    /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
    rtsConfig.outOfHeapHook(0/*unknown request size*/,
                            (W_)RtsFlags.GcFlags.maxHeapSize * BLOCK_SIZE);
}

void
exitHeapOverflow(void)
{
    reportHeapOverflow();  // reportHeapOverflow() doesn't exit (see #2592)
    stg_exit(EXIT_HEAPOVERFLOW);
}

/* -----------------------------------------------------------------------------
   Sleep for the given period of time.
   -------------------------------------------------------------------------- */

/* Returns -1 on failure but handles EINTR internally. On Windows this will
 * only have millisecond precision. */
int rtsSleep(Time t)
{
#if defined(_WIN32)
    // N.B. we can't use nanosleep on Windows as it would incur a pthreads
    // dependency. See #18272.
    Sleep(TimeToMS(t));
    return 0;
#else
    struct timespec req;
    req.tv_sec = TimeToSeconds(t);
    req.tv_nsec = TimeToNS(t - req.tv_sec * TIME_RESOLUTION);
    int ret;
    do {
        ret = nanosleep(&req, &req);
    } while (ret == -1 && errno == EINTR);
    return ret;
#endif /* _WIN32 */
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
#if defined(HAVE_CTIME_R)
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
showStgWord64(StgWord64 x, char *s, bool with_commas)
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
#if defined(DEBUG)
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
#elif defined(HAVE_RAISE)
        return raise(sig);
#else
        exit(sig);
#endif
}

static void mkRtsInfoPair(const char *key, const char *val) {
    /* XXX should check for "s, \s etc in key and val */
    printf(" ,(\"%s\", \"%s\")\n", key, val);
}

/* This little bit of magic allows us to say TOSTRING(SYM) and get
 * "5" if SYM is 5 */
#define TOSTRING2(x) #x
#define TOSTRING(x)  TOSTRING2(x)

void printRtsInfo(const RtsConfig rts_config) {
    /* The first entry is just a hack to make it easy to get the
     * commas right */
    printf(" [(\"GHC RTS\", \"YES\")\n");
    mkRtsInfoPair("GHC version",             ProjectVersion);
    mkRtsInfoPair("RTS way",                 RtsWay);
    mkRtsInfoPair("Host platform",           HostPlatform);
    mkRtsInfoPair("Host architecture",       HostArch);
    mkRtsInfoPair("Host OS",                 HostOS);
    mkRtsInfoPair("Host vendor",             HostVendor);
    mkRtsInfoPair("Word size",               TOSTRING(WORD_SIZE_IN_BITS));
    // TODO(@Ericson2314) This is a joint property of the RTS and generated
    // code. The compiler will soon be multi-target so it doesn't make sense to
    // say the target is <ABI adj>, unless we are talking about the host
    // platform of the compiler / ABI used by a compiler plugin. This is *not*
    // that, so I think a rename is in order to avoid confusion.
    mkRtsInfoPair("Compiler unregisterised", GhcUnregisterised);
    mkRtsInfoPair("Tables next to code",     TablesNextToCode);
    mkRtsInfoPair("Flag -with-rtsopts",      /* See #15261 */
        rts_config.rts_opts != NULL ? rts_config.rts_opts : "");
    selectIOManager(); /* resolve the io-manager, accounting for flags  */
    mkRtsInfoPair("I/O manager default",     showIOManager());
    printf(" ]\n");
}

// Provides a way for Haskell programs to tell whether they're being
// profiled or not.  GHCi uses it (see #2197).
int rts_isProfiled(void)
{
#if defined(PROFILING)
    return 1;
#else
    return 0;
#endif
}

// Provides a way for Haskell programs to tell whether they're
// dynamically-linked or not.
int rts_isDynamic(void)
{
#if defined(DYNAMIC)
    return 1;
#else
    return 0;
#endif
}

// Provides a way for Haskell programs to tell whether they're
// linked with the threaded runtime or not.
int rts_isThreaded(void)
{
#if defined(THREADED_RTS)
    return 1;
#else
    return 0;
#endif
}

// Provides a way for Haskell programs to tell whether they're
// linked with the debug runtime or not.
int rts_isDebugged(void)
{
#if defined(DEBUG)
    return 1;
#else
    return 0;
#endif
}

// Provides a way for Haskell programs to tell whether they're
// linked with the tracing runtime or not.
int rts_isTracing(void)
{
#if defined(TRACING)
    return 1;
#else
    return 0;
#endif
}

// Used for detecting a non-empty FPU stack on x86 (see #4914)
void checkFPUStack(void)
{
#if defined(i386_HOST_ARCH)
    static unsigned char buf[108];
    asm("FSAVE %0":"=m" (buf));

    if(buf[8]!=255 || buf[9]!=255) {
        errorBelch("NONEMPTY FPU Stack, TAG = %x %x\n",buf[8],buf[9]);
        abort();
    }
#endif
}

// Drop the given extension from a filepath.
void dropExtension(char *path, const char *extension) {
    int ext_len = strlen(extension);
    int path_len = strlen(path);
    if (ext_len < path_len) {
        char *s = &path[path_len - ext_len];
        if (strcmp(s, extension) == 0) {
            *s = '\0';
        }
    }
}
