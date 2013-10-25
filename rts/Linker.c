/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2012
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#if 0
#include "PosixSource.h"
#endif

/* Linux needs _GNU_SOURCE to get RTLD_DEFAULT from <dlfcn.h> and
   MREMAP_MAYMOVE from <sys/mman.h>.
 */
#if defined(__linux__)  || defined(__GLIBC__)
#define _GNU_SOURCE 1
#endif

#include "Rts.h"
#include "HsFFI.h"

#include "sm/Storage.h"
#include "Stats.h"
#include "Hash.h"
#include "LinkerInternals.h"
#include "RtsUtils.h"
#include "Trace.h"
#include "StgPrimFloat.h" // for __int_encodeFloat etc.
#include "Proftimer.h"
#include "GetEnv.h"
#include "Stable.h"

#if !defined(mingw32_HOST_OS)
#include "posix/Signals.h"
#endif

// get protos for is*()
#include <ctype.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if defined(HAVE_DLFCN_H)
#include <dlfcn.h>
#endif

#if defined(cygwin32_HOST_OS)
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <regex.h>
#include <sys/fcntl.h>
#include <sys/termios.h>
#include <sys/utime.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#endif

#if (defined(powerpc_HOST_ARCH) && defined(linux_HOST_OS)) \
 || (!defined(powerpc_HOST_ARCH) && \
    (   defined(linux_HOST_OS)     || defined(freebsd_HOST_OS) || \
        defined(dragonfly_HOST_OS) || defined(netbsd_HOST_OS ) || \
        defined(openbsd_HOST_OS  ) || defined(darwin_HOST_OS ) || \
        defined(kfreebsdgnu_HOST_OS) || defined(gnu_HOST_OS)))
/* Don't use mmap on powerpc_HOST_ARCH as mmap doesn't support
 * reallocating but we need to allocate jump islands just after each
 * object images. Otherwise relative branches to jump islands can fail
 * due to 24-bits displacement overflow.
 */
#define USE_MMAP
#include <fcntl.h>
#include <sys/mman.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#endif


/* PowerPC has relative branch instructions with only 24 bit displacements
 * and therefore needs jump islands contiguous with each object code module.
 */
#if (defined(USE_MMAP) && defined(powerpc_HOST_ARCH) && defined(linux_HOST_OS))
#define USE_CONTIGUOUS_MMAP 1
#else
#define USE_CONTIGUOUS_MMAP 0
#endif

#if defined(linux_HOST_OS) || defined(solaris2_HOST_OS) || defined(freebsd_HOST_OS) || defined(kfreebsdgnu_HOST_OS) || defined(dragonfly_HOST_OS) || defined(netbsd_HOST_OS) || defined(openbsd_HOST_OS) || defined(gnu_HOST_OS)
#  define OBJFORMAT_ELF
#  include <regex.h>    // regex is already used by dlopen() so this is OK
                        // to use here without requiring an additional lib
#elif defined(cygwin32_HOST_OS) || defined (mingw32_HOST_OS)
#  define OBJFORMAT_PEi386
#  include <windows.h>
#  include <math.h>
#elif defined(darwin_HOST_OS)
#  define OBJFORMAT_MACHO
#  include <regex.h>
#  include <mach/machine.h>
#  include <mach-o/fat.h>
#  include <mach-o/loader.h>
#  include <mach-o/nlist.h>
#  include <mach-o/reloc.h>
#if !defined(HAVE_DLFCN_H)
#  include <mach-o/dyld.h>
#endif
#if defined(powerpc_HOST_ARCH)
#  include <mach-o/ppc/reloc.h>
#endif
#if defined(x86_64_HOST_ARCH)
#  include <mach-o/x86_64/reloc.h>
#endif
#endif

#if defined(x86_64_HOST_ARCH) && defined(darwin_HOST_OS)
#define ALWAYS_PIC
#endif

#if defined(dragonfly_HOST_OS)
#include <sys/tls.h>
#endif

typedef struct _RtsSymbolInfo {
    void *value;
    const ObjectCode *owner;
    HsBool weak;
} RtsSymbolInfo;

/* Hash table mapping symbol names to RtsSymbolInfo */
static /*Str*/HashTable *symhash;

/* List of currently loaded objects */
ObjectCode *objects = NULL;     /* initially empty */

/* List of objects that have been unloaded via unloadObj(), but are waiting
   to be actually freed via checkUnload() */
ObjectCode *unloaded_objects = NULL; /* initially empty */

/* Type of the initializer */
typedef void (*init_t) (int argc, char **argv, char **env);

static HsInt loadOc( ObjectCode* oc );
static ObjectCode* mkOc( pathchar *path, char *image, int imageSize,
                         char *archiveMemberName
#ifndef USE_MMAP
#ifdef darwin_HOST_OS
                       , int misalignment
#endif
#endif
                       );

// Use wchar_t for pathnames on Windows (#5697)
#if defined(mingw32_HOST_OS)
#define pathcmp wcscmp
#define pathlen wcslen
#define pathopen _wfopen
#define pathstat _wstat
#define struct_stat struct _stat
#define open wopen
#define WSTR(s) L##s
#else
#define pathcmp strcmp
#define pathlen strlen
#define pathopen fopen
#define pathstat stat
#define struct_stat struct stat
#define WSTR(s) s
#endif

static pathchar* pathdup(pathchar *path)
{
    pathchar *ret;
#if defined(mingw32_HOST_OS)
    ret = wcsdup(path);
#else
    /* sigh, strdup() isn't a POSIX function, so do it the long way */
    ret = stgMallocBytes( strlen(path)+1, "loadObj" );
    strcpy(ret, path);
#endif
    return ret;
}


#if defined(OBJFORMAT_ELF)
static int ocVerifyImage_ELF    ( ObjectCode* oc );
static int ocGetNames_ELF       ( ObjectCode* oc );
static int ocResolve_ELF        ( ObjectCode* oc );
static int ocRunInit_ELF        ( ObjectCode* oc );
#if defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH) || defined(arm_HOST_ARCH)
static int ocAllocateSymbolExtras_ELF ( ObjectCode* oc );
#endif
#elif defined(OBJFORMAT_PEi386)
static int ocVerifyImage_PEi386 ( ObjectCode* oc );
static int ocGetNames_PEi386    ( ObjectCode* oc );
static int ocResolve_PEi386     ( ObjectCode* oc );
static int ocRunInit_PEi386     ( ObjectCode* oc );
static void *lookupSymbolInDLLs ( unsigned char *lbl );
static void zapTrailingAtSign   ( unsigned char *sym );
#elif defined(OBJFORMAT_MACHO)
static int ocVerifyImage_MachO    ( ObjectCode* oc );
static int ocGetNames_MachO       ( ObjectCode* oc );
static int ocResolve_MachO        ( ObjectCode* oc );
static int ocRunInit_MachO        ( ObjectCode* oc );

#ifndef USE_MMAP
static int machoGetMisalignment( FILE * );
#endif
#if defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
static int ocAllocateSymbolExtras_MachO ( ObjectCode* oc );
#endif
#ifdef powerpc_HOST_ARCH
static void machoInitSymbolsWithoutUnderscore( void );
#endif
#endif

static void freeProddableBlocks (ObjectCode *oc);

/* on x86_64 we have a problem with relocating symbol references in
 * code that was compiled without -fPIC.  By default, the small memory
 * model is used, which assumes that symbol references can fit in a
 * 32-bit slot.  The system dynamic linker makes this work for
 * references to shared libraries by either (a) allocating a jump
 * table slot for code references, or (b) moving the symbol at load
 * time (and copying its contents, if necessary) for data references.
 *
 * We unfortunately can't tell whether symbol references are to code
 * or data.  So for now we assume they are code (the vast majority
 * are), and allocate jump-table slots.  Unfortunately this will
 * SILENTLY generate crashing code for data references.  This hack is
 * enabled by X86_64_ELF_NONPIC_HACK.
 *
 * One workaround is to use shared Haskell libraries.  This is
 * coming.  Another workaround is to keep the static libraries but
 * compile them with -fPIC, because that will generate PIC references
 * to data which can be relocated.  The PIC code is still too green to
 * do this systematically, though.
 *
 * See bug #781
 * See thread http://www.haskell.org/pipermail/cvs-ghc/2007-September/038458.html
 *
 * Naming Scheme for Symbol Macros
 *
 * SymI_*: symbol is internal to the RTS. It resides in an object
 *         file/library that is statically.
 * SymE_*: symbol is external to the RTS library. It might be linked
 *         dynamically.
 *
 * Sym*_HasProto  : the symbol prototype is imported in an include file
 *                  or defined explicitly
 * Sym*_NeedsProto: the symbol is undefined and we add a dummy
 *                  default proto extern void sym(void);
 */
#define X86_64_ELF_NONPIC_HACK 1

/* Link objects into the lower 2Gb on x86_64.  GHC assumes the
 * small memory model on this architecture (see gcc docs,
 * -mcmodel=small).
 *
 * MAP_32BIT not available on OpenBSD/amd64
 */
#if defined(x86_64_HOST_ARCH) && defined(MAP_32BIT)
#define TRY_MAP_32BIT MAP_32BIT
#else
#define TRY_MAP_32BIT 0
#endif

/*
 * Due to the small memory model (see above), on x86_64 we have to map
 * all our non-PIC object files into the low 2Gb of the address space
 * (why 2Gb and not 4Gb?  Because all addresses must be reachable
 * using a 32-bit signed PC-relative offset). On Linux we can do this
 * using the MAP_32BIT flag to mmap(), however on other OSs
 * (e.g. *BSD, see #2063, and also on Linux inside Xen, see #2512), we
 * can't do this.  So on these systems, we have to pick a base address
 * in the low 2Gb of the address space and try to allocate memory from
 * there.
 *
 * We pick a default address based on the OS, but also make this
 * configurable via an RTS flag (+RTS -xm)
 */
#if !defined(ALWAYS_PIC) && defined(x86_64_HOST_ARCH)

#if defined(MAP_32BIT)
// Try to use MAP_32BIT
#define MMAP_32BIT_BASE_DEFAULT 0
#else
// A guess: 1Gb.
#define MMAP_32BIT_BASE_DEFAULT 0x40000000
#endif

static void *mmap_32bit_base = (void *)MMAP_32BIT_BASE_DEFAULT;
#endif

/* MAP_ANONYMOUS is MAP_ANON on some systems, e.g. OpenBSD */
#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS MAP_ANON
#endif

/* -----------------------------------------------------------------------------
 * Built-in symbols from the RTS
 */

typedef struct _RtsSymbolVal {
    char   *lbl;
    void   *addr;
} RtsSymbolVal;

#define Maybe_Stable_Names      SymI_HasProto(stg_mkWeakzh)                     \
                                SymI_HasProto(stg_mkWeakNoFinalizzerzh)         \
                                SymI_HasProto(stg_addCFinalizzerToWeakzh)       \
                                SymI_HasProto(stg_makeStableNamezh)             \
                                SymI_HasProto(stg_finalizzeWeakzh)

#if !defined (mingw32_HOST_OS)
#define RTS_POSIX_ONLY_SYMBOLS                  \
      SymI_HasProto(__hscore_get_saved_termios) \
      SymI_HasProto(__hscore_set_saved_termios) \
      SymI_HasProto(shutdownHaskellAndSignal)   \
      SymI_HasProto(signal_handlers)            \
      SymI_HasProto(stg_sig_install)            \
      SymI_HasProto(rtsTimerSignal)             \
      SymI_HasProto(atexit)                     \
      SymI_NeedsProto(nocldstop)
#endif

#if defined (cygwin32_HOST_OS)
#define RTS_MINGW_ONLY_SYMBOLS /**/
/* Don't have the ability to read import libs / archives, so
 * we have to stupidly list a lot of what libcygwin.a
 * exports; sigh.
 */
#define RTS_CYGWIN_ONLY_SYMBOLS                          \
      SymI_HasProto(regfree)                             \
      SymI_HasProto(regexec)                             \
      SymI_HasProto(regerror)                            \
      SymI_HasProto(regcomp)                             \
      SymI_HasProto(__errno)                             \
      SymI_HasProto(access)                              \
      SymI_HasProto(chmod)                               \
      SymI_HasProto(chdir)                               \
      SymI_HasProto(close)                               \
      SymI_HasProto(creat)                               \
      SymI_HasProto(dup)                                 \
      SymI_HasProto(dup2)                                \
      SymI_HasProto(fstat)                               \
      SymI_HasProto(fcntl)                               \
      SymI_HasProto(getcwd)                              \
      SymI_HasProto(getenv)                              \
      SymI_HasProto(lseek)                               \
      SymI_HasProto(open)                                \
      SymI_HasProto(fpathconf)                           \
      SymI_HasProto(pathconf)                            \
      SymI_HasProto(stat)                                \
      SymI_HasProto(pow)                                 \
      SymI_HasProto(tanh)                                \
      SymI_HasProto(cosh)                                \
      SymI_HasProto(sinh)                                \
      SymI_HasProto(atan)                                \
      SymI_HasProto(acos)                                \
      SymI_HasProto(asin)                                \
      SymI_HasProto(tan)                                 \
      SymI_HasProto(cos)                                 \
      SymI_HasProto(sin)                                 \
      SymI_HasProto(exp)                                 \
      SymI_HasProto(log)                                 \
      SymI_HasProto(sqrt)                                \
      SymI_HasProto(localtime_r)                         \
      SymI_HasProto(gmtime_r)                            \
      SymI_HasProto(mktime)                              \
      SymI_NeedsProto(_imp___tzname)                     \
      SymI_HasProto(gettimeofday)                        \
      SymI_HasProto(timezone)                            \
      SymI_HasProto(tcgetattr)                           \
      SymI_HasProto(tcsetattr)                           \
      SymI_HasProto(memcpy)                              \
      SymI_HasProto(memmove)                             \
      SymI_HasProto(realloc)                             \
      SymI_HasProto(malloc)                              \
      SymI_HasProto(free)                                \
      SymI_HasProto(fork)                                \
      SymI_HasProto(lstat)                               \
      SymI_HasProto(isatty)                              \
      SymI_HasProto(mkdir)                               \
      SymI_HasProto(opendir)                             \
      SymI_HasProto(readdir)                             \
      SymI_HasProto(rewinddir)                           \
      SymI_HasProto(closedir)                            \
      SymI_HasProto(link)                                \
      SymI_HasProto(mkfifo)                              \
      SymI_HasProto(pipe)                                \
      SymI_HasProto(read)                                \
      SymI_HasProto(rename)                              \
      SymI_HasProto(rmdir)                               \
      SymI_HasProto(select)                              \
      SymI_HasProto(system)                              \
      SymI_HasProto(write)                               \
      SymI_HasProto(strcmp)                              \
      SymI_HasProto(strcpy)                              \
      SymI_HasProto(strncpy)                             \
      SymI_HasProto(strerror)                            \
      SymI_HasProto(sigaddset)                           \
      SymI_HasProto(sigemptyset)                         \
      SymI_HasProto(sigprocmask)                         \
      SymI_HasProto(umask)                               \
      SymI_HasProto(uname)                               \
      SymI_HasProto(unlink)                              \
      SymI_HasProto(utime)                               \
      SymI_HasProto(waitpid)

#elif defined(mingw32_HOST_OS)
#define RTS_POSIX_ONLY_SYMBOLS  /**/
#define RTS_CYGWIN_ONLY_SYMBOLS /**/

#if HAVE_GETTIMEOFDAY
#define RTS_MINGW_GETTIMEOFDAY_SYM SymI_NeedsProto(gettimeofday)
#else
#define RTS_MINGW_GETTIMEOFDAY_SYM /**/
#endif

#if HAVE___MINGW_VFPRINTF
#define RTS___MINGW_VFPRINTF_SYM SymI_HasProto(__mingw_vfprintf)
#else
#define RTS___MINGW_VFPRINTF_SYM /**/
#endif

#if defined(i386_HOST_ARCH)
#define RTS_WIN32_ONLY(X) X
#else
#define RTS_WIN32_ONLY(X) /**/
#endif

#if defined(x86_64_HOST_ARCH)
#define RTS_WIN64_ONLY(X) X
#else
#define RTS_WIN64_ONLY(X) /**/
#endif

/* These are statically linked from the mingw libraries into the ghc
   executable, so we have to employ this hack. */
#define RTS_MINGW_ONLY_SYMBOLS                           \
      SymI_HasProto(stg_asyncReadzh)                     \
      SymI_HasProto(stg_asyncWritezh)                    \
      SymI_HasProto(stg_asyncDoProczh)                   \
      SymI_HasProto(getWin32ProgArgv)                    \
      SymI_HasProto(setWin32ProgArgv)                    \
      SymI_HasProto(memset)                              \
      SymI_HasProto(inet_ntoa)                           \
      SymI_HasProto(inet_addr)                           \
      SymI_HasProto(htonl)                               \
      SymI_HasProto(recvfrom)                            \
      SymI_HasProto(listen)                              \
      SymI_HasProto(bind)                                \
      SymI_HasProto(shutdown)                            \
      SymI_HasProto(connect)                             \
      SymI_HasProto(htons)                               \
      SymI_HasProto(ntohs)                               \
      SymI_HasProto(getservbyname)                       \
      SymI_HasProto(getservbyport)                       \
      SymI_HasProto(getprotobynumber)                    \
      SymI_HasProto(getprotobyname)                      \
      SymI_HasProto(gethostbyname)                       \
      SymI_HasProto(gethostbyaddr)                       \
      SymI_HasProto(gethostname)                         \
      SymI_HasProto(strcpy)                              \
      SymI_HasProto(strncpy)                             \
      SymI_HasProto(abort)                               \
      RTS_WIN32_ONLY(SymI_NeedsProto(_alloca))           \
      SymI_HasProto(isxdigit)                            \
      SymI_HasProto(isupper)                             \
      SymI_HasProto(ispunct)                             \
      SymI_HasProto(islower)                             \
      SymI_HasProto(isspace)                             \
      SymI_HasProto(isprint)                             \
      SymI_HasProto(isdigit)                             \
      SymI_HasProto(iscntrl)                             \
      SymI_HasProto(isalpha)                             \
      SymI_HasProto(isalnum)                             \
      SymI_HasProto(isascii)                             \
      RTS___MINGW_VFPRINTF_SYM                           \
      SymI_HasProto(strcmp)                              \
      SymI_HasProto(memmove)                             \
      SymI_HasProto(realloc)                             \
      SymI_HasProto(malloc)                              \
      SymI_HasProto(pow)                                 \
      SymI_HasProto(tanh)                                \
      SymI_HasProto(cosh)                                \
      SymI_HasProto(sinh)                                \
      SymI_HasProto(atan)                                \
      SymI_HasProto(acos)                                \
      SymI_HasProto(asin)                                \
      SymI_HasProto(tan)                                 \
      SymI_HasProto(cos)                                 \
      SymI_HasProto(sin)                                 \
      SymI_HasProto(exp)                                 \
      SymI_HasProto(log)                                 \
      SymI_HasProto(sqrt)                                \
      SymI_HasProto(powf)                                \
      SymI_HasProto(tanhf)                               \
      SymI_HasProto(coshf)                               \
      SymI_HasProto(sinhf)                               \
      SymI_HasProto(atanf)                               \
      SymI_HasProto(acosf)                               \
      SymI_HasProto(asinf)                               \
      SymI_HasProto(tanf)                                \
      SymI_HasProto(cosf)                                \
      SymI_HasProto(sinf)                                \
      SymI_HasProto(expf)                                \
      SymI_HasProto(logf)                                \
      SymI_HasProto(sqrtf)                               \
      SymI_HasProto(erf)                                 \
      SymI_HasProto(erfc)                                \
      SymI_HasProto(erff)                                \
      SymI_HasProto(erfcf)                               \
      SymI_HasProto(memcpy)                              \
      SymI_HasProto(rts_InstallConsoleEvent)             \
      SymI_HasProto(rts_ConsoleHandlerDone)              \
      SymI_NeedsProto(mktime)                            \
      RTS_WIN32_ONLY(SymI_NeedsProto(_imp___timezone))   \
      RTS_WIN32_ONLY(SymI_NeedsProto(_imp___tzname))     \
      RTS_WIN32_ONLY(SymI_NeedsProto(_imp__tzname))      \
      RTS_WIN32_ONLY(SymI_NeedsProto(_imp___iob))        \
      RTS_WIN32_ONLY(SymI_NeedsProto(_imp___osver))      \
      SymI_NeedsProto(localtime)                         \
      SymI_NeedsProto(gmtime)                            \
      SymI_NeedsProto(opendir)                           \
      SymI_NeedsProto(readdir)                           \
      SymI_NeedsProto(rewinddir)                         \
      RTS_WIN32_ONLY(SymI_NeedsProto(_imp____mb_cur_max)) \
      RTS_WIN32_ONLY(SymI_NeedsProto(_imp___pctype))     \
      RTS_WIN32_ONLY(SymI_NeedsProto(__chkstk))          \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp___iob_func))  \
      RTS_WIN64_ONLY(SymI_NeedsProto(___chkstk_ms))      \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_localeconv))  \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_islower))     \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_isspace))     \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_isxdigit))    \
      RTS_WIN64_ONLY(SymI_HasProto(close))               \
      RTS_WIN64_ONLY(SymI_HasProto(read))                \
      RTS_WIN64_ONLY(SymI_HasProto(dup))                 \
      RTS_WIN64_ONLY(SymI_HasProto(dup2))                \
      RTS_WIN64_ONLY(SymI_HasProto(write))               \
      SymI_NeedsProto(getpid)                            \
      RTS_WIN64_ONLY(SymI_HasProto(access))              \
      SymI_HasProto(chmod)                               \
      RTS_WIN64_ONLY(SymI_HasProto(creat))               \
      RTS_WIN64_ONLY(SymI_HasProto(umask))               \
      SymI_HasProto(unlink)                              \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp__errno))      \
      RTS_WIN64_ONLY(SymI_NeedsProto(ftruncate64))       \
      RTS_WIN64_ONLY(SymI_HasProto(setmode))             \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp__wstat64))    \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp__fstat64))    \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp__wsopen))     \
      RTS_WIN64_ONLY(SymI_HasProto(__imp__environ))      \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_GetFileAttributesA))          \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_GetFileInformationByHandle))  \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_GetFileType))                 \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_GetLastError))                \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_QueryPerformanceFrequency))   \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_QueryPerformanceCounter))     \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_GetTickCount))                \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_WaitForSingleObject))         \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_PeekConsoleInputA))           \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_ReadConsoleInputA))           \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_PeekNamedPipe))               \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp__isatty))                     \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_select))                      \
      RTS_WIN64_ONLY(SymI_HasProto(isatty))                              \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp__get_osfhandle))              \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_GetConsoleMode))              \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_SetConsoleMode))              \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_FlushConsoleInputBuffer))     \
      RTS_WIN64_ONLY(SymI_HasProto(free))                                \
      RTS_WIN64_ONLY(SymI_NeedsProto(raise))                             \
      RTS_WIN64_ONLY(SymI_NeedsProto(_getpid))                           \
      RTS_WIN64_ONLY(SymI_HasProto(getc))                                \
      RTS_WIN64_ONLY(SymI_HasProto(ungetc))                              \
      RTS_WIN64_ONLY(SymI_HasProto(puts))                                \
      RTS_WIN64_ONLY(SymI_HasProto(putc))                                \
      RTS_WIN64_ONLY(SymI_HasProto(putchar))                             \
      RTS_WIN64_ONLY(SymI_HasProto(fputc))                               \
      RTS_WIN64_ONLY(SymI_HasProto(fread))                               \
      RTS_WIN64_ONLY(SymI_HasProto(fwrite))                              \
      RTS_WIN64_ONLY(SymI_HasProto(ferror))                              \
      RTS_WIN64_ONLY(SymI_HasProto(printf))                              \
      RTS_WIN64_ONLY(SymI_HasProto(fprintf))                             \
      RTS_WIN64_ONLY(SymI_HasProto(sprintf))                             \
      RTS_WIN64_ONLY(SymI_HasProto(vsprintf))                            \
      RTS_WIN64_ONLY(SymI_HasProto(sscanf))                              \
      RTS_WIN64_ONLY(SymI_HasProto(ldexp))                               \
      RTS_WIN64_ONLY(SymI_HasProto(strlen))                              \
      RTS_WIN64_ONLY(SymI_HasProto(strnlen))                             \
      RTS_WIN64_ONLY(SymI_HasProto(strchr))                              \
      RTS_WIN64_ONLY(SymI_HasProto(strtol))                              \
      RTS_WIN64_ONLY(SymI_HasProto(strerror))                            \
      RTS_WIN64_ONLY(SymI_HasProto(memchr))                              \
      RTS_WIN64_ONLY(SymI_HasProto(memcmp))                              \
      RTS_WIN64_ONLY(SymI_HasProto(wcscpy))                              \
      RTS_WIN64_ONLY(SymI_HasProto(wcslen))                              \
      RTS_WIN64_ONLY(SymI_HasProto(_lseeki64))                           \
      RTS_WIN64_ONLY(SymI_HasProto(_wchmod))                             \
      RTS_WIN64_ONLY(SymI_HasProto(closesocket))                         \
      RTS_WIN64_ONLY(SymI_HasProto(send))                                \
      RTS_WIN64_ONLY(SymI_HasProto(recv))                                \
      RTS_WIN64_ONLY(SymI_HasProto(bsearch))                             \
      RTS_WIN64_ONLY(SymI_HasProto(CommandLineToArgvW))                  \
      RTS_WIN64_ONLY(SymI_HasProto(CreateBitmap))                        \
      RTS_WIN64_ONLY(SymI_HasProto(CreateBitmapIndirect))                \
      RTS_WIN64_ONLY(SymI_HasProto(CreateCompatibleBitmap))              \
      RTS_WIN64_ONLY(SymI_HasProto(CreateDIBPatternBrushPt))             \
      RTS_WIN64_ONLY(SymI_HasProto(CreateDIBitmap))                      \
      RTS_WIN64_ONLY(SymI_HasProto(SetBitmapDimensionEx))                \
      RTS_WIN64_ONLY(SymI_HasProto(GetBitmapDimensionEx))                \
      RTS_WIN64_ONLY(SymI_HasProto(GetStockObject))                      \
      RTS_WIN64_ONLY(SymI_HasProto(GetObjectW))                          \
      RTS_WIN64_ONLY(SymI_HasProto(DeleteObject))                        \
      RTS_WIN64_ONLY(SymI_HasProto(SetDIBits))                           \
      RTS_WIN64_ONLY(SymI_HasProto(GetDIBits))                           \
      RTS_WIN64_ONLY(SymI_HasProto(CreateSolidBrush))                    \
      RTS_WIN64_ONLY(SymI_HasProto(CreateHatchBrush))                    \
      RTS_WIN64_ONLY(SymI_HasProto(CreatePatternBrush))                  \
      RTS_WIN64_ONLY(SymI_HasProto(CreateFontW))                         \
      RTS_WIN64_ONLY(SymI_HasProto(AngleArc)) \
      RTS_WIN64_ONLY(SymI_HasProto(Arc)) \
      RTS_WIN64_ONLY(SymI_HasProto(ArcTo)) \
      RTS_WIN64_ONLY(SymI_HasProto(BeginPath)) \
      RTS_WIN64_ONLY(SymI_HasProto(BitBlt)) \
      RTS_WIN64_ONLY(SymI_HasProto(CancelDC)) \
      RTS_WIN64_ONLY(SymI_HasProto(Chord)) \
      RTS_WIN64_ONLY(SymI_HasProto(CloseFigure)) \
      RTS_WIN64_ONLY(SymI_HasProto(CombineRgn)) \
      RTS_WIN64_ONLY(SymI_HasProto(CreateCompatibleDC)) \
      RTS_WIN64_ONLY(SymI_HasProto(CreateEllipticRgn)) \
      RTS_WIN64_ONLY(SymI_HasProto(CreateEllipticRgnIndirect)) \
      RTS_WIN64_ONLY(SymI_HasProto(CreatePen)) \
      RTS_WIN64_ONLY(SymI_HasProto(CreatePolygonRgn)) \
      RTS_WIN64_ONLY(SymI_HasProto(CreateRectRgn)) \
      RTS_WIN64_ONLY(SymI_HasProto(CreateRectRgnIndirect)) \
      RTS_WIN64_ONLY(SymI_HasProto(CreateRoundRectRgn)) \
      RTS_WIN64_ONLY(SymI_HasProto(DeleteDC)) \
      RTS_WIN64_ONLY(SymI_HasProto(Ellipse)) \
      RTS_WIN64_ONLY(SymI_HasProto(EndPath)) \
      RTS_WIN64_ONLY(SymI_HasProto(EqualRgn)) \
      RTS_WIN64_ONLY(SymI_HasProto(ExtSelectClipRgn)) \
      RTS_WIN64_ONLY(SymI_HasProto(FillPath)) \
      RTS_WIN64_ONLY(SymI_HasProto(FillRgn)) \
      RTS_WIN64_ONLY(SymI_HasProto(FlattenPath)) \
      RTS_WIN64_ONLY(SymI_HasProto(FrameRgn)) \
      RTS_WIN64_ONLY(SymI_HasProto(GetArcDirection)) \
      RTS_WIN64_ONLY(SymI_HasProto(GetBkColor)) \
      RTS_WIN64_ONLY(SymI_HasProto(GetBkMode)) \
      RTS_WIN64_ONLY(SymI_HasProto(GetBrushOrgEx)) \
      RTS_WIN64_ONLY(SymI_HasProto(GetCurrentObject)) \
      RTS_WIN64_ONLY(SymI_HasProto(GetDCOrgEx)) \
      RTS_WIN64_ONLY(SymI_HasProto(GetGraphicsMode)) \
      RTS_WIN64_ONLY(SymI_HasProto(GetMiterLimit)) \
      RTS_WIN64_ONLY(SymI_HasProto(GetPolyFillMode)) \
      RTS_WIN64_ONLY(SymI_HasProto(GetRgnBox)) \
      RTS_WIN64_ONLY(SymI_HasProto(GetStretchBltMode)) \
      RTS_WIN64_ONLY(SymI_HasProto(GetTextAlign)) \
      RTS_WIN64_ONLY(SymI_HasProto(GetTextCharacterExtra)) \
      RTS_WIN64_ONLY(SymI_HasProto(GetTextColor)) \
      RTS_WIN64_ONLY(SymI_HasProto(GetTextExtentPoint32W)) \
      RTS_WIN64_ONLY(SymI_HasProto(InvertRgn)) \
      RTS_WIN64_ONLY(SymI_HasProto(LineTo)) \
      RTS_WIN64_ONLY(SymI_HasProto(MaskBlt)) \
      RTS_WIN64_ONLY(SymI_HasProto(MoveToEx)) \
      RTS_WIN64_ONLY(SymI_HasProto(OffsetRgn)) \
      RTS_WIN64_ONLY(SymI_HasProto(PaintRgn)) \
      RTS_WIN64_ONLY(SymI_HasProto(PathToRegion)) \
      RTS_WIN64_ONLY(SymI_HasProto(Pie)) \
      RTS_WIN64_ONLY(SymI_HasProto(PlgBlt)) \
      RTS_WIN64_ONLY(SymI_HasProto(PolyBezier)) \
      RTS_WIN64_ONLY(SymI_HasProto(PolyBezierTo)) \
      RTS_WIN64_ONLY(SymI_HasProto(Polygon)) \
      RTS_WIN64_ONLY(SymI_HasProto(Polyline)) \
      RTS_WIN64_ONLY(SymI_HasProto(PolylineTo)) \
      RTS_WIN64_ONLY(SymI_HasProto(PtInRegion)) \
      RTS_WIN64_ONLY(SymI_HasProto(Rectangle)) \
      RTS_WIN64_ONLY(SymI_HasProto(RectInRegion)) \
      RTS_WIN64_ONLY(SymI_HasProto(RestoreDC)) \
      RTS_WIN64_ONLY(SymI_HasProto(RoundRect)) \
      RTS_WIN64_ONLY(SymI_HasProto(SaveDC)) \
      RTS_WIN64_ONLY(SymI_HasProto(SelectClipPath)) \
      RTS_WIN64_ONLY(SymI_HasProto(SelectClipRgn)) \
      RTS_WIN64_ONLY(SymI_HasProto(SelectObject)) \
      RTS_WIN64_ONLY(SymI_HasProto(SelectPalette)) \
      RTS_WIN64_ONLY(SymI_HasProto(SetArcDirection)) \
      RTS_WIN64_ONLY(SymI_HasProto(SetBkColor)) \
      RTS_WIN64_ONLY(SymI_HasProto(SetBkMode)) \
      RTS_WIN64_ONLY(SymI_HasProto(SetBrushOrgEx)) \
      RTS_WIN64_ONLY(SymI_HasProto(SetGraphicsMode)) \
      RTS_WIN64_ONLY(SymI_HasProto(SetMiterLimit)) \
      RTS_WIN64_ONLY(SymI_HasProto(SetPolyFillMode)) \
      RTS_WIN64_ONLY(SymI_HasProto(SetStretchBltMode)) \
      RTS_WIN64_ONLY(SymI_HasProto(SetTextAlign)) \
      RTS_WIN64_ONLY(SymI_HasProto(SetTextCharacterExtra)) \
      RTS_WIN64_ONLY(SymI_HasProto(SetTextColor)) \
      RTS_WIN64_ONLY(SymI_HasProto(StretchBlt)) \
      RTS_WIN64_ONLY(SymI_HasProto(StrokeAndFillPath)) \
      RTS_WIN64_ONLY(SymI_HasProto(StrokePath)) \
      RTS_WIN64_ONLY(SymI_HasProto(TextOutW)) \
      RTS_WIN64_ONLY(SymI_HasProto(timeGetTime)) \
      RTS_WIN64_ONLY(SymI_HasProto(WidenPath)) \
      RTS_WIN64_ONLY(SymI_HasProto(GetFileSecurityW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegCloseKey)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegConnectRegistryW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegCreateKeyExW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegCreateKeyW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegDeleteKeyW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegDeleteValueW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegEnumKeyW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegEnumValueW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegFlushKey)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegLoadKeyW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegNotifyChangeKeyValue)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegOpenKeyExW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegOpenKeyW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegQueryInfoKeyW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegQueryValueExW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegQueryValueW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegReplaceKeyW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegRestoreKeyW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegSaveKeyW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegSetValueExW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegSetValueW)) \
      RTS_WIN64_ONLY(SymI_HasProto(RegUnLoadKeyW)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(SHGetFolderPathW)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_SetWindowLongPtrW)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_GetWindowLongPtrW)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_MenuItemFromPoint)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_ChildWindowFromPoint)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_ChildWindowFromPointEx)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_DeleteObject)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_UnmapViewOfFile)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_CloseHandle)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_FreeLibrary)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_GetMessageW)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_TranslateMessage)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_DispatchMessageW)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_DefWindowProcW)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_GetDIBits)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_GlobalAlloc)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_GlobalFree)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_CreateFileW)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_WriteFile)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_CreateCompatibleBitmap)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_SelectObject)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_Polygon)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_FormatMessageW)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp__localtime64)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp__tzname)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp__timezone)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_CreatePipe)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_SetHandleInformation)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_GetStdHandle)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_GetCurrentProcess)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_DuplicateHandle)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_CreateProcessW)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_TerminateProcess)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp__open_osfhandle)) \
      RTS_WIN64_ONLY(SymI_NeedsProto(__imp_GetExitCodeProcess)) \
      RTS_MINGW_GETTIMEOFDAY_SYM                         \
      SymI_NeedsProto(closedir)

#else
#define RTS_MINGW_ONLY_SYMBOLS /**/
#define RTS_CYGWIN_ONLY_SYMBOLS /**/
#endif


#if defined(darwin_HOST_OS) && HAVE_PRINTF_LDBLSTUB
#define RTS_DARWIN_ONLY_SYMBOLS                             \
     SymI_NeedsProto(asprintf$LDBLStub)                     \
     SymI_NeedsProto(err$LDBLStub)                          \
     SymI_NeedsProto(errc$LDBLStub)                         \
     SymI_NeedsProto(errx$LDBLStub)                         \
     SymI_NeedsProto(fprintf$LDBLStub)                      \
     SymI_NeedsProto(fscanf$LDBLStub)                       \
     SymI_NeedsProto(fwprintf$LDBLStub)                     \
     SymI_NeedsProto(fwscanf$LDBLStub)                      \
     SymI_NeedsProto(printf$LDBLStub)                       \
     SymI_NeedsProto(scanf$LDBLStub)                        \
     SymI_NeedsProto(snprintf$LDBLStub)                     \
     SymI_NeedsProto(sprintf$LDBLStub)                      \
     SymI_NeedsProto(sscanf$LDBLStub)                       \
     SymI_NeedsProto(strtold$LDBLStub)                      \
     SymI_NeedsProto(swprintf$LDBLStub)                     \
     SymI_NeedsProto(swscanf$LDBLStub)                      \
     SymI_NeedsProto(syslog$LDBLStub)                       \
     SymI_NeedsProto(vasprintf$LDBLStub)                    \
     SymI_NeedsProto(verr$LDBLStub)                         \
     SymI_NeedsProto(verrc$LDBLStub)                        \
     SymI_NeedsProto(verrx$LDBLStub)                        \
     SymI_NeedsProto(vfprintf$LDBLStub)                     \
     SymI_NeedsProto(vfscanf$LDBLStub)                      \
     SymI_NeedsProto(vfwprintf$LDBLStub)                    \
     SymI_NeedsProto(vfwscanf$LDBLStub)                     \
     SymI_NeedsProto(vprintf$LDBLStub)                      \
     SymI_NeedsProto(vscanf$LDBLStub)                       \
     SymI_NeedsProto(vsnprintf$LDBLStub)                    \
     SymI_NeedsProto(vsprintf$LDBLStub)                     \
     SymI_NeedsProto(vsscanf$LDBLStub)                      \
     SymI_NeedsProto(vswprintf$LDBLStub)                    \
     SymI_NeedsProto(vswscanf$LDBLStub)                     \
     SymI_NeedsProto(vsyslog$LDBLStub)                      \
     SymI_NeedsProto(vwarn$LDBLStub)                        \
     SymI_NeedsProto(vwarnc$LDBLStub)                       \
     SymI_NeedsProto(vwarnx$LDBLStub)                       \
     SymI_NeedsProto(vwprintf$LDBLStub)                     \
     SymI_NeedsProto(vwscanf$LDBLStub)                      \
     SymI_NeedsProto(warn$LDBLStub)                         \
     SymI_NeedsProto(warnc$LDBLStub)                        \
     SymI_NeedsProto(warnx$LDBLStub)                        \
     SymI_NeedsProto(wcstold$LDBLStub)                      \
     SymI_NeedsProto(wprintf$LDBLStub)                      \
     SymI_NeedsProto(wscanf$LDBLStub)
#else
#define RTS_DARWIN_ONLY_SYMBOLS
#endif

#ifndef SMP
# define MAIN_CAP_SYM SymI_HasProto(MainCapability)
#else
# define MAIN_CAP_SYM
#endif

#if !defined(mingw32_HOST_OS)
#define RTS_USER_SIGNALS_SYMBOLS        \
   SymI_HasProto(setIOManagerControlFd) \
   SymI_HasProto(setIOManagerWakeupFd)  \
   SymI_HasProto(ioManagerWakeup)       \
   SymI_HasProto(blockUserSignals)      \
   SymI_HasProto(unblockUserSignals)
#else
#define RTS_USER_SIGNALS_SYMBOLS        \
   SymI_HasProto(ioManagerWakeup)       \
   SymI_HasProto(sendIOManagerEvent)    \
   SymI_HasProto(readIOManagerEvent)    \
   SymI_HasProto(getIOManagerEvent)     \
   SymI_HasProto(console_handler)
#endif

#define RTS_LIBFFI_SYMBOLS                                  \
     SymE_NeedsProto(ffi_prep_cif)                          \
     SymE_NeedsProto(ffi_call)                              \
     SymE_NeedsProto(ffi_type_void)                         \
     SymE_NeedsProto(ffi_type_float)                        \
     SymE_NeedsProto(ffi_type_double)                       \
     SymE_NeedsProto(ffi_type_sint64)                       \
     SymE_NeedsProto(ffi_type_uint64)                       \
     SymE_NeedsProto(ffi_type_sint32)                       \
     SymE_NeedsProto(ffi_type_uint32)                       \
     SymE_NeedsProto(ffi_type_sint16)                       \
     SymE_NeedsProto(ffi_type_uint16)                       \
     SymE_NeedsProto(ffi_type_sint8)                        \
     SymE_NeedsProto(ffi_type_uint8)                        \
     SymE_NeedsProto(ffi_type_pointer)

#ifdef TABLES_NEXT_TO_CODE
#define RTS_RET_SYMBOLS /* nothing */
#else
#define RTS_RET_SYMBOLS                                 \
      SymI_HasProto(stg_enter_ret)                      \
      SymI_HasProto(stg_gc_fun_ret)                     \
      SymI_HasProto(stg_ap_v_ret)                       \
      SymI_HasProto(stg_ap_f_ret)                       \
      SymI_HasProto(stg_ap_d_ret)                       \
      SymI_HasProto(stg_ap_l_ret)                       \
      SymI_HasProto(stg_ap_v16_ret)                     \
      SymI_HasProto(stg_ap_v32_ret)                     \
      SymI_HasProto(stg_ap_v64_ret)                     \
      SymI_HasProto(stg_ap_n_ret)                       \
      SymI_HasProto(stg_ap_p_ret)                       \
      SymI_HasProto(stg_ap_pv_ret)                      \
      SymI_HasProto(stg_ap_pp_ret)                      \
      SymI_HasProto(stg_ap_ppv_ret)                     \
      SymI_HasProto(stg_ap_ppp_ret)                     \
      SymI_HasProto(stg_ap_pppv_ret)                    \
      SymI_HasProto(stg_ap_pppp_ret)                    \
      SymI_HasProto(stg_ap_ppppp_ret)                   \
      SymI_HasProto(stg_ap_pppppp_ret)
#endif

/* Modules compiled with -ticky may mention ticky counters */
/* This list should marry up with the one in $(TOP)/includes/stg/Ticky.h */
#define RTS_TICKY_SYMBOLS                               \
      SymI_NeedsProto(ticky_entry_ctrs)                 \
      SymI_NeedsProto(top_ct)                           \
                                                        \
      SymI_HasProto(ENT_VIA_NODE_ctr)                   \
      SymI_HasProto(ENT_STATIC_THK_SINGLE_ctr)          \
      SymI_HasProto(ENT_STATIC_THK_MANY_ctr)            \
      SymI_HasProto(ENT_DYN_THK_SINGLE_ctr)             \
      SymI_HasProto(ENT_DYN_THK_MANY_ctr)               \
      SymI_HasProto(ENT_STATIC_FUN_DIRECT_ctr)          \
      SymI_HasProto(ENT_DYN_FUN_DIRECT_ctr)             \
      SymI_HasProto(ENT_STATIC_CON_ctr)                 \
      SymI_HasProto(ENT_DYN_CON_ctr)                    \
      SymI_HasProto(ENT_STATIC_IND_ctr)                 \
      SymI_HasProto(ENT_DYN_IND_ctr)                    \
      SymI_HasProto(ENT_PERM_IND_ctr)                   \
      SymI_HasProto(ENT_PAP_ctr)                        \
      SymI_HasProto(ENT_AP_ctr)                         \
      SymI_HasProto(ENT_AP_STACK_ctr)                   \
      SymI_HasProto(ENT_BH_ctr)                         \
      SymI_HasProto(ENT_LNE_ctr)                        \
      SymI_HasProto(UNKNOWN_CALL_ctr)                   \
      SymI_HasProto(SLOW_CALL_fast_v16_ctr)                  \
      SymI_HasProto(SLOW_CALL_fast_v_ctr)                    \
      SymI_HasProto(SLOW_CALL_fast_f_ctr)                    \
      SymI_HasProto(SLOW_CALL_fast_d_ctr)                    \
      SymI_HasProto(SLOW_CALL_fast_l_ctr)                    \
      SymI_HasProto(SLOW_CALL_fast_n_ctr)                    \
      SymI_HasProto(SLOW_CALL_fast_p_ctr)                    \
      SymI_HasProto(SLOW_CALL_fast_pv_ctr)                   \
      SymI_HasProto(SLOW_CALL_fast_pp_ctr)                   \
      SymI_HasProto(SLOW_CALL_fast_ppv_ctr)                  \
      SymI_HasProto(SLOW_CALL_fast_ppp_ctr)                  \
      SymI_HasProto(SLOW_CALL_fast_pppv_ctr)                 \
      SymI_HasProto(SLOW_CALL_fast_pppp_ctr)                 \
      SymI_HasProto(SLOW_CALL_fast_ppppp_ctr)                \
      SymI_HasProto(SLOW_CALL_fast_pppppp_ctr)               \
      SymI_HasProto(VERY_SLOW_CALL_ctr)                \
      SymI_HasProto(ticky_slow_call_unevald)            \
      SymI_HasProto(SLOW_CALL_ctr)                      \
      SymI_HasProto(MULTI_CHUNK_SLOW_CALL_ctr)          \
      SymI_HasProto(MULTI_CHUNK_SLOW_CALL_CHUNKS_ctr)   \
      SymI_HasProto(KNOWN_CALL_ctr)                     \
      SymI_HasProto(KNOWN_CALL_TOO_FEW_ARGS_ctr)        \
      SymI_HasProto(KNOWN_CALL_EXTRA_ARGS_ctr)          \
      SymI_HasProto(SLOW_CALL_FUN_TOO_FEW_ctr)          \
      SymI_HasProto(SLOW_CALL_FUN_CORRECT_ctr)          \
      SymI_HasProto(SLOW_CALL_FUN_TOO_MANY_ctr)         \
      SymI_HasProto(SLOW_CALL_PAP_TOO_FEW_ctr)          \
      SymI_HasProto(SLOW_CALL_PAP_CORRECT_ctr)          \
      SymI_HasProto(SLOW_CALL_PAP_TOO_MANY_ctr)         \
      SymI_HasProto(SLOW_CALL_UNEVALD_ctr)              \
      SymI_HasProto(UPDF_OMITTED_ctr)                   \
      SymI_HasProto(UPDF_PUSHED_ctr)                    \
      SymI_HasProto(CATCHF_PUSHED_ctr)                  \
      SymI_HasProto(UPDF_RCC_PUSHED_ctr)                \
      SymI_HasProto(UPDF_RCC_OMITTED_ctr)               \
      SymI_HasProto(UPD_SQUEEZED_ctr)                   \
      SymI_HasProto(UPD_CON_IN_NEW_ctr)                 \
      SymI_HasProto(UPD_CON_IN_PLACE_ctr)               \
      SymI_HasProto(UPD_PAP_IN_NEW_ctr)                 \
      SymI_HasProto(UPD_PAP_IN_PLACE_ctr)               \
      SymI_HasProto(ALLOC_HEAP_ctr)                     \
      SymI_HasProto(ALLOC_HEAP_tot)                     \
      SymI_HasProto(HEAP_CHK_ctr)                       \
      SymI_HasProto(STK_CHK_ctr)                        \
      SymI_HasProto(ALLOC_RTS_ctr)                      \
      SymI_HasProto(ALLOC_RTS_tot)                      \
      SymI_HasProto(ALLOC_FUN_ctr)                      \
      SymI_HasProto(ALLOC_FUN_adm)                      \
      SymI_HasProto(ALLOC_FUN_gds)                      \
      SymI_HasProto(ALLOC_FUN_slp)                      \
      SymI_HasProto(UPD_NEW_IND_ctr)                    \
      SymI_HasProto(UPD_NEW_PERM_IND_ctr)               \
      SymI_HasProto(UPD_OLD_IND_ctr)                    \
      SymI_HasProto(UPD_OLD_PERM_IND_ctr)               \
      SymI_HasProto(UPD_CAF_BH_UPDATABLE_ctr)           \
      SymI_HasProto(UPD_CAF_BH_SINGLE_ENTRY_ctr)        \
      SymI_HasProto(GC_SEL_ABANDONED_ctr)               \
      SymI_HasProto(GC_SEL_MINOR_ctr)                   \
      SymI_HasProto(GC_SEL_MAJOR_ctr)                   \
      SymI_HasProto(GC_FAILED_PROMOTION_ctr)            \
      SymI_HasProto(ALLOC_UP_THK_ctr)                   \
      SymI_HasProto(ALLOC_SE_THK_ctr)                   \
      SymI_HasProto(ALLOC_THK_adm)                      \
      SymI_HasProto(ALLOC_THK_gds)                      \
      SymI_HasProto(ALLOC_THK_slp)                      \
      SymI_HasProto(ALLOC_CON_ctr)                      \
      SymI_HasProto(ALLOC_CON_adm)                      \
      SymI_HasProto(ALLOC_CON_gds)                      \
      SymI_HasProto(ALLOC_CON_slp)                      \
      SymI_HasProto(ALLOC_TUP_ctr)                      \
      SymI_HasProto(ALLOC_TUP_adm)                      \
      SymI_HasProto(ALLOC_TUP_gds)                      \
      SymI_HasProto(ALLOC_TUP_slp)                      \
      SymI_HasProto(ALLOC_BH_ctr)                       \
      SymI_HasProto(ALLOC_BH_adm)                       \
      SymI_HasProto(ALLOC_BH_gds)                       \
      SymI_HasProto(ALLOC_BH_slp)                       \
      SymI_HasProto(ALLOC_PRIM_ctr)                     \
      SymI_HasProto(ALLOC_PRIM_adm)                     \
      SymI_HasProto(ALLOC_PRIM_gds)                     \
      SymI_HasProto(ALLOC_PRIM_slp)                     \
      SymI_HasProto(ALLOC_PAP_ctr)                      \
      SymI_HasProto(ALLOC_PAP_adm)                      \
      SymI_HasProto(ALLOC_PAP_gds)                      \
      SymI_HasProto(ALLOC_PAP_slp)                      \
      SymI_HasProto(ALLOC_TSO_ctr)                      \
      SymI_HasProto(ALLOC_TSO_adm)                      \
      SymI_HasProto(ALLOC_TSO_gds)                      \
      SymI_HasProto(ALLOC_TSO_slp)                      \
      SymI_HasProto(RET_NEW_ctr)                        \
      SymI_HasProto(RET_OLD_ctr)                        \
      SymI_HasProto(RET_UNBOXED_TUP_ctr)                \
      SymI_HasProto(RET_SEMI_loads_avoided)


// On most platforms, the garbage collector rewrites references
//      to small integer and char objects to a set of common, shared ones.
//
// We don't do this when compiling to Windows DLLs at the moment because
//      it doesn't support cross package data references well.
//
#if defined(COMPILING_WINDOWS_DLL)
#define RTS_INTCHAR_SYMBOLS
#else
#define RTS_INTCHAR_SYMBOLS                             \
      SymI_HasProto(stg_CHARLIKE_closure)               \
      SymI_HasProto(stg_INTLIKE_closure)
#endif


#define RTS_SYMBOLS                                                     \
      Maybe_Stable_Names                                                \
      RTS_TICKY_SYMBOLS                                                 \
      SymI_HasProto(StgReturn)                                          \
      SymI_HasProto(stg_gc_noregs)                                      \
      SymI_HasProto(stg_ret_v_info)                                     \
      SymI_HasProto(stg_ret_p_info)                                     \
      SymI_HasProto(stg_ret_n_info)                                     \
      SymI_HasProto(stg_ret_f_info)                                     \
      SymI_HasProto(stg_ret_d_info)                                     \
      SymI_HasProto(stg_ret_l_info)                                     \
      SymI_HasProto(stg_gc_prim_p)                                      \
      SymI_HasProto(stg_gc_prim_pp)                                     \
      SymI_HasProto(stg_gc_prim_n)                                      \
      SymI_HasProto(stg_enter_info)                                     \
      SymI_HasProto(__stg_gc_enter_1)                                   \
      SymI_HasProto(stg_gc_unpt_r1)                                     \
      SymI_HasProto(stg_gc_unbx_r1)                                     \
      SymI_HasProto(stg_gc_f1)                                          \
      SymI_HasProto(stg_gc_d1)                                          \
      SymI_HasProto(stg_gc_l1)                                          \
      SymI_HasProto(stg_gc_pp)                                          \
      SymI_HasProto(stg_gc_ppp)                                         \
      SymI_HasProto(stg_gc_pppp)                                        \
      SymI_HasProto(__stg_gc_fun)                                       \
      SymI_HasProto(stg_gc_fun_info)                                    \
      SymI_HasProto(stg_yield_noregs)                                   \
      SymI_HasProto(stg_yield_to_interpreter)                           \
      SymI_HasProto(stg_block_noregs)                                   \
      SymI_HasProto(stg_block_takemvar)                                 \
      SymI_HasProto(stg_block_readmvar)                           \
      SymI_HasProto(stg_block_putmvar)                                  \
      MAIN_CAP_SYM                                                      \
      SymI_HasProto(MallocFailHook)                                     \
      SymI_HasProto(OnExitHook)                                         \
      SymI_HasProto(OutOfHeapHook)                                      \
      SymI_HasProto(StackOverflowHook)                                  \
      SymI_HasProto(addDLL)                                             \
      SymI_HasProto(__int_encodeDouble)                                 \
      SymI_HasProto(__word_encodeDouble)                                \
      SymI_HasProto(__int_encodeFloat)                                  \
      SymI_HasProto(__word_encodeFloat)                                 \
      SymI_HasProto(stg_atomicallyzh)                                   \
      SymI_HasProto(barf)                                               \
      SymI_HasProto(debugBelch)                                         \
      SymI_HasProto(errorBelch)                                         \
      SymI_HasProto(sysErrorBelch)                                      \
      SymI_HasProto(stg_getMaskingStatezh)                              \
      SymI_HasProto(stg_maskAsyncExceptionszh)                          \
      SymI_HasProto(stg_maskUninterruptiblezh)                          \
      SymI_HasProto(stg_catchzh)                                        \
      SymI_HasProto(stg_catchRetryzh)                                   \
      SymI_HasProto(stg_catchSTMzh)                                     \
      SymI_HasProto(stg_checkzh)                                        \
      SymI_HasProto(closure_flags)                                      \
      SymI_HasProto(cmp_thread)                                         \
      SymI_HasProto(createAdjustor)                                     \
      SymI_HasProto(stg_decodeDoublezu2Intzh)                           \
      SymI_HasProto(stg_decodeFloatzuIntzh)                             \
      SymI_HasProto(defaultsHook)                                       \
      SymI_HasProto(stg_delayzh)                                        \
      SymI_HasProto(stg_deRefWeakzh)                                    \
      SymI_HasProto(stg_deRefStablePtrzh)                               \
      SymI_HasProto(dirty_MUT_VAR)                                      \
      SymI_HasProto(dirty_TVAR)                                         \
      SymI_HasProto(stg_forkzh)                                         \
      SymI_HasProto(stg_forkOnzh)                                       \
      SymI_HasProto(forkProcess)                                        \
      SymI_HasProto(forkOS_createThread)                                \
      SymI_HasProto(freeHaskellFunctionPtr)                             \
      SymI_HasProto(getOrSetGHCConcSignalSignalHandlerStore)            \
      SymI_HasProto(getOrSetGHCConcWindowsPendingDelaysStore)           \
      SymI_HasProto(getOrSetGHCConcWindowsIOManagerThreadStore)         \
      SymI_HasProto(getOrSetGHCConcWindowsProddingStore)                \
      SymI_HasProto(getOrSetSystemEventThreadEventManagerStore)         \
      SymI_HasProto(getOrSetSystemEventThreadIOManagerThreadStore)      \
      SymI_HasProto(getOrSetSystemTimerThreadEventManagerStore)         \
      SymI_HasProto(getOrSetSystemTimerThreadIOManagerThreadStore)      \
      SymI_HasProto(getOrSetLibHSghcFastStringTable)                    \
      SymI_HasProto(getGCStats)                                         \
      SymI_HasProto(getGCStatsEnabled)                                  \
      SymI_HasProto(genericRaise)                                       \
      SymI_HasProto(getProgArgv)                                        \
      SymI_HasProto(getFullProgArgv)                                    \
      SymI_HasProto(getStablePtr)                                       \
      SymI_HasProto(foreignExportStablePtr)                             \
      SymI_HasProto(hs_init)                                            \
      SymI_HasProto(hs_exit)                                            \
      SymI_HasProto(hs_set_argv)                                        \
      SymI_HasProto(hs_add_root)                                        \
      SymI_HasProto(hs_perform_gc)                                      \
      SymI_HasProto(hs_lock_stable_tables)                              \
      SymI_HasProto(hs_unlock_stable_tables)                            \
      SymI_HasProto(hs_free_stable_ptr)                                 \
      SymI_HasProto(hs_free_stable_ptr_unsafe)                          \
      SymI_HasProto(hs_free_fun_ptr)                                    \
      SymI_HasProto(hs_hpc_rootModule)                                  \
      SymI_HasProto(hs_hpc_module)                                      \
      SymI_HasProto(initLinker)                                         \
      SymI_HasProto(stg_unpackClosurezh)                                \
      SymI_HasProto(stg_getApStackValzh)                                \
      SymI_HasProto(stg_getSparkzh)                                     \
      SymI_HasProto(stg_numSparkszh)                                    \
      SymI_HasProto(stg_isCurrentThreadBoundzh)                         \
      SymI_HasProto(stg_isEmptyMVarzh)                                  \
      SymI_HasProto(stg_killThreadzh)                                   \
      SymI_HasProto(loadArchive)                                        \
      SymI_HasProto(loadObj)                                            \
      SymI_HasProto(insertSymbol)                                       \
      SymI_HasProto(lookupSymbol)                                       \
      SymI_HasProto(stg_makeStablePtrzh)                                \
      SymI_HasProto(stg_mkApUpd0zh)                                     \
      SymI_HasProto(stg_myThreadIdzh)                                   \
      SymI_HasProto(stg_labelThreadzh)                                  \
      SymI_HasProto(stg_newArrayzh)                                     \
      SymI_HasProto(stg_newArrayArrayzh)                                \
      SymI_HasProto(stg_casArrayzh)                                     \
      SymI_HasProto(stg_newBCOzh)                                       \
      SymI_HasProto(stg_newByteArrayzh)                                 \
      SymI_HasProto(stg_casIntArrayzh)                                  \
      SymI_HasProto(stg_fetchAddIntArrayzh)                             \
      SymI_HasProto_redirect(newCAF, newDynCAF)                         \
      SymI_HasProto(stg_newMVarzh)                                      \
      SymI_HasProto(stg_newMutVarzh)                                    \
      SymI_HasProto(stg_newTVarzh)                                      \
      SymI_HasProto(stg_noDuplicatezh)                                  \
      SymI_HasProto(stg_atomicModifyMutVarzh)                           \
      SymI_HasProto(stg_casMutVarzh)                                    \
      SymI_HasProto(stg_newPinnedByteArrayzh)                           \
      SymI_HasProto(stg_newAlignedPinnedByteArrayzh)                    \
      SymI_HasProto(newSpark)                                           \
      SymI_HasProto(performGC)                                          \
      SymI_HasProto(performMajorGC)                                     \
      SymI_HasProto(prog_argc)                                          \
      SymI_HasProto(prog_argv)                                          \
      SymI_HasProto(stg_putMVarzh)                                      \
      SymI_HasProto(stg_raisezh)                                        \
      SymI_HasProto(stg_raiseIOzh)                                      \
      SymI_HasProto(stg_readTVarzh)                                     \
      SymI_HasProto(stg_readTVarIOzh)                                   \
      SymI_HasProto(resumeThread)                                       \
      SymI_HasProto(setNumCapabilities)                                 \
      SymI_HasProto(getNumberOfProcessors)                              \
      SymI_HasProto(resolveObjs)                                        \
      SymI_HasProto(stg_retryzh)                                        \
      SymI_HasProto(rts_apply)                                          \
      SymI_HasProto(rts_checkSchedStatus)                               \
      SymI_HasProto(rts_eval)                                           \
      SymI_HasProto(rts_evalIO)                                         \
      SymI_HasProto(rts_evalLazyIO)                                     \
      SymI_HasProto(rts_evalStableIO)                                   \
      SymI_HasProto(rts_eval_)                                          \
      SymI_HasProto(rts_getBool)                                        \
      SymI_HasProto(rts_getChar)                                        \
      SymI_HasProto(rts_getDouble)                                      \
      SymI_HasProto(rts_getFloat)                                       \
      SymI_HasProto(rts_getInt)                                         \
      SymI_HasProto(rts_getInt8)                                        \
      SymI_HasProto(rts_getInt16)                                       \
      SymI_HasProto(rts_getInt32)                                       \
      SymI_HasProto(rts_getInt64)                                       \
      SymI_HasProto(rts_getPtr)                                         \
      SymI_HasProto(rts_getFunPtr)                                      \
      SymI_HasProto(rts_getStablePtr)                                   \
      SymI_HasProto(rts_getThreadId)                                    \
      SymI_HasProto(rts_getWord)                                        \
      SymI_HasProto(rts_getWord8)                                       \
      SymI_HasProto(rts_getWord16)                                      \
      SymI_HasProto(rts_getWord32)                                      \
      SymI_HasProto(rts_getWord64)                                      \
      SymI_HasProto(rts_lock)                                           \
      SymI_HasProto(rts_mkBool)                                         \
      SymI_HasProto(rts_mkChar)                                         \
      SymI_HasProto(rts_mkDouble)                                       \
      SymI_HasProto(rts_mkFloat)                                        \
      SymI_HasProto(rts_mkInt)                                          \
      SymI_HasProto(rts_mkInt8)                                         \
      SymI_HasProto(rts_mkInt16)                                        \
      SymI_HasProto(rts_mkInt32)                                        \
      SymI_HasProto(rts_mkInt64)                                        \
      SymI_HasProto(rts_mkPtr)                                          \
      SymI_HasProto(rts_mkFunPtr)                                       \
      SymI_HasProto(rts_mkStablePtr)                                    \
      SymI_HasProto(rts_mkString)                                       \
      SymI_HasProto(rts_mkWord)                                         \
      SymI_HasProto(rts_mkWord8)                                        \
      SymI_HasProto(rts_mkWord16)                                       \
      SymI_HasProto(rts_mkWord32)                                       \
      SymI_HasProto(rts_mkWord64)                                       \
      SymI_HasProto(rts_unlock)                                         \
      SymI_HasProto(rts_unsafeGetMyCapability)                          \
      SymI_HasProto(rtsSupportsBoundThreads)                            \
      SymI_HasProto(rts_isProfiled)                                     \
      SymI_HasProto(rts_isDynamic)                                      \
      SymI_HasProto(setProgArgv)                                        \
      SymI_HasProto(startupHaskell)                                     \
      SymI_HasProto(shutdownHaskell)                                    \
      SymI_HasProto(shutdownHaskellAndExit)                             \
      SymI_HasProto(stable_name_table)                                  \
      SymI_HasProto(stable_ptr_table)                                   \
      SymI_HasProto(stackOverflow)                                      \
      SymI_HasProto(stg_CAF_BLACKHOLE_info)                             \
      SymI_HasProto(stg_BLACKHOLE_info)                                 \
      SymI_HasProto(__stg_EAGER_BLACKHOLE_info)                         \
      SymI_HasProto(stg_BLOCKING_QUEUE_CLEAN_info)                      \
      SymI_HasProto(stg_BLOCKING_QUEUE_DIRTY_info)                      \
      SymI_HasProto(startTimer)                                         \
      SymI_HasProto(stg_MVAR_CLEAN_info)                                \
      SymI_HasProto(stg_MVAR_DIRTY_info)                                \
      SymI_HasProto(stg_TVAR_CLEAN_info)                                \
      SymI_HasProto(stg_TVAR_DIRTY_info)                                \
      SymI_HasProto(stg_IND_STATIC_info)                                \
      SymI_HasProto(stg_ARR_WORDS_info)                                 \
      SymI_HasProto(stg_MUT_ARR_PTRS_DIRTY_info)                        \
      SymI_HasProto(stg_MUT_ARR_PTRS_FROZEN_info)                       \
      SymI_HasProto(stg_MUT_ARR_PTRS_FROZEN0_info)                      \
      SymI_HasProto(stg_MUT_VAR_CLEAN_info)                             \
      SymI_HasProto(stg_MUT_VAR_DIRTY_info)                             \
      SymI_HasProto(stg_WEAK_info)                                      \
      SymI_HasProto(stg_ap_v_info)                                      \
      SymI_HasProto(stg_ap_f_info)                                      \
      SymI_HasProto(stg_ap_d_info)                                      \
      SymI_HasProto(stg_ap_l_info)                                      \
      SymI_HasProto(stg_ap_v16_info)                                    \
      SymI_HasProto(stg_ap_v32_info)                                    \
      SymI_HasProto(stg_ap_v64_info)                                    \
      SymI_HasProto(stg_ap_n_info)                                      \
      SymI_HasProto(stg_ap_p_info)                                      \
      SymI_HasProto(stg_ap_pv_info)                                     \
      SymI_HasProto(stg_ap_pp_info)                                     \
      SymI_HasProto(stg_ap_ppv_info)                                    \
      SymI_HasProto(stg_ap_ppp_info)                                    \
      SymI_HasProto(stg_ap_pppv_info)                                   \
      SymI_HasProto(stg_ap_pppp_info)                                   \
      SymI_HasProto(stg_ap_ppppp_info)                                  \
      SymI_HasProto(stg_ap_pppppp_info)                                 \
      SymI_HasProto(stg_ap_0_fast)                                      \
      SymI_HasProto(stg_ap_v_fast)                                      \
      SymI_HasProto(stg_ap_f_fast)                                      \
      SymI_HasProto(stg_ap_d_fast)                                      \
      SymI_HasProto(stg_ap_l_fast)                                      \
      SymI_HasProto(stg_ap_v16_fast)                                    \
      SymI_HasProto(stg_ap_v32_fast)                                    \
      SymI_HasProto(stg_ap_v64_fast)                                    \
      SymI_HasProto(stg_ap_n_fast)                                      \
      SymI_HasProto(stg_ap_p_fast)                                      \
      SymI_HasProto(stg_ap_pv_fast)                                     \
      SymI_HasProto(stg_ap_pp_fast)                                     \
      SymI_HasProto(stg_ap_ppv_fast)                                    \
      SymI_HasProto(stg_ap_ppp_fast)                                    \
      SymI_HasProto(stg_ap_pppv_fast)                                   \
      SymI_HasProto(stg_ap_pppp_fast)                                   \
      SymI_HasProto(stg_ap_ppppp_fast)                                  \
      SymI_HasProto(stg_ap_pppppp_fast)                                 \
      SymI_HasProto(stg_ap_1_upd_info)                                  \
      SymI_HasProto(stg_ap_2_upd_info)                                  \
      SymI_HasProto(stg_ap_3_upd_info)                                  \
      SymI_HasProto(stg_ap_4_upd_info)                                  \
      SymI_HasProto(stg_ap_5_upd_info)                                  \
      SymI_HasProto(stg_ap_6_upd_info)                                  \
      SymI_HasProto(stg_ap_7_upd_info)                                  \
      SymI_HasProto(stg_exit)                                           \
      SymI_HasProto(stg_sel_0_upd_info)                                 \
      SymI_HasProto(stg_sel_1_upd_info)                                 \
      SymI_HasProto(stg_sel_2_upd_info)                                 \
      SymI_HasProto(stg_sel_3_upd_info)                                 \
      SymI_HasProto(stg_sel_4_upd_info)                                 \
      SymI_HasProto(stg_sel_5_upd_info)                                 \
      SymI_HasProto(stg_sel_6_upd_info)                                 \
      SymI_HasProto(stg_sel_7_upd_info)                                 \
      SymI_HasProto(stg_sel_8_upd_info)                                 \
      SymI_HasProto(stg_sel_9_upd_info)                                 \
      SymI_HasProto(stg_sel_10_upd_info)                                \
      SymI_HasProto(stg_sel_11_upd_info)                                \
      SymI_HasProto(stg_sel_12_upd_info)                                \
      SymI_HasProto(stg_sel_13_upd_info)                                \
      SymI_HasProto(stg_sel_14_upd_info)                                \
      SymI_HasProto(stg_sel_15_upd_info)                                \
      SymI_HasProto(stg_sel_0_noupd_info)                                 \
      SymI_HasProto(stg_sel_1_noupd_info)                                 \
      SymI_HasProto(stg_sel_2_noupd_info)                                 \
      SymI_HasProto(stg_sel_3_noupd_info)                                 \
      SymI_HasProto(stg_sel_4_noupd_info)                                 \
      SymI_HasProto(stg_sel_5_noupd_info)                                 \
      SymI_HasProto(stg_sel_6_noupd_info)                                 \
      SymI_HasProto(stg_sel_7_noupd_info)                                 \
      SymI_HasProto(stg_sel_8_noupd_info)                                 \
      SymI_HasProto(stg_sel_9_noupd_info)                                 \
      SymI_HasProto(stg_sel_10_noupd_info)                                \
      SymI_HasProto(stg_sel_11_noupd_info)                                \
      SymI_HasProto(stg_sel_12_noupd_info)                                \
      SymI_HasProto(stg_sel_13_noupd_info)                                \
      SymI_HasProto(stg_sel_14_noupd_info)                                \
      SymI_HasProto(stg_sel_15_noupd_info)                                \
      SymI_HasProto(stg_upd_frame_info)                                 \
      SymI_HasProto(stg_bh_upd_frame_info)                              \
      SymI_HasProto(suspendThread)                                      \
      SymI_HasProto(stg_takeMVarzh)                                     \
      SymI_HasProto(stg_readMVarzh)                               \
      SymI_HasProto(stg_threadStatuszh)                                 \
      SymI_HasProto(stg_tryPutMVarzh)                                   \
      SymI_HasProto(stg_tryTakeMVarzh)                                  \
      SymI_HasProto(stg_tryReadMVarzh)                            \
      SymI_HasProto(stg_unmaskAsyncExceptionszh)                        \
      SymI_HasProto(unloadObj)                                          \
      SymI_HasProto(stg_unsafeThawArrayzh)                              \
      SymI_HasProto(stg_waitReadzh)                                     \
      SymI_HasProto(stg_waitWritezh)                                    \
      SymI_HasProto(stg_writeTVarzh)                                    \
      SymI_HasProto(stg_yieldzh)                                        \
      SymI_NeedsProto(stg_interp_constr_entry)                          \
      SymI_HasProto(stg_arg_bitmaps)                                    \
      SymI_HasProto(large_alloc_lim)                                    \
      SymI_HasProto(g0)                                                 \
      SymI_HasProto(allocate)                                           \
      SymI_HasProto(allocateExec)                                       \
      SymI_HasProto(freeExec)                                           \
      SymI_HasProto(getAllocations)                                     \
      SymI_HasProto(revertCAFs)                                         \
      SymI_HasProto(RtsFlags)                                           \
      SymI_NeedsProto(rts_breakpoint_io_action)                         \
      SymI_NeedsProto(rts_stop_next_breakpoint)                         \
      SymI_NeedsProto(rts_stop_on_exception)                            \
      SymI_HasProto(stopTimer)                                          \
      SymI_HasProto(n_capabilities)                                     \
      SymI_HasProto(enabled_capabilities)                               \
      SymI_HasProto(stg_traceCcszh)                                     \
      SymI_HasProto(stg_traceEventzh)                                   \
      SymI_HasProto(stg_traceMarkerzh)                                  \
      SymI_HasProto(getMonotonicNSec)                                   \
      SymI_HasProto(lockFile)                                           \
      SymI_HasProto(unlockFile)                                         \
      SymI_HasProto(startProfTimer)                                     \
      SymI_HasProto(stopProfTimer)                                      \
      SymI_HasProto(atomic_inc)                                         \
      SymI_HasProto(atomic_dec)                                         \
      RTS_USER_SIGNALS_SYMBOLS                                          \
      RTS_INTCHAR_SYMBOLS


// 64-bit support functions in libgcc.a
#if defined(__GNUC__) && SIZEOF_VOID_P <= 4 && !defined(_ABIN32)
#define RTS_LIBGCC_SYMBOLS                             \
      SymI_NeedsProto(__divdi3)                        \
      SymI_NeedsProto(__udivdi3)                       \
      SymI_NeedsProto(__moddi3)                        \
      SymI_NeedsProto(__umoddi3)                       \
      SymI_NeedsProto(__muldi3)                        \
      SymI_NeedsProto(__ashldi3)                       \
      SymI_NeedsProto(__ashrdi3)                       \
      SymI_NeedsProto(__lshrdi3)                       \
      SymI_NeedsProto(__fixunsdfdi)
#else
#define RTS_LIBGCC_SYMBOLS
#endif

#if defined(darwin_HOST_OS) && defined(powerpc_HOST_ARCH)
      // Symbols that don't have a leading underscore
      // on Mac OS X. They have to receive special treatment,
      // see machoInitSymbolsWithoutUnderscore()
#define RTS_MACHO_NOUNDERLINE_SYMBOLS                   \
      SymI_NeedsProto(saveFP)                           \
      SymI_NeedsProto(restFP)
#endif

/* entirely bogus claims about types of these symbols */
#define SymI_NeedsProto(vvv)  extern void vvv(void);
#if defined(COMPILING_WINDOWS_DLL)
#define SymE_HasProto(vvv)    SymE_HasProto(vvv);
#  if defined(x86_64_HOST_ARCH)
#    define SymE_NeedsProto(vvv)    extern void __imp_ ## vvv (void);
#  else
#    define SymE_NeedsProto(vvv)    extern void _imp__ ## vvv (void);
#  endif
#else
#define SymE_NeedsProto(vvv)  SymI_NeedsProto(vvv);
#define SymE_HasProto(vvv)    SymI_HasProto(vvv)
#endif
#define SymI_HasProto(vvv) /**/
#define SymI_HasProto_redirect(vvv,xxx) /**/
RTS_SYMBOLS
RTS_RET_SYMBOLS
RTS_POSIX_ONLY_SYMBOLS
RTS_MINGW_ONLY_SYMBOLS
RTS_CYGWIN_ONLY_SYMBOLS
RTS_DARWIN_ONLY_SYMBOLS
RTS_LIBGCC_SYMBOLS
RTS_LIBFFI_SYMBOLS
#undef SymI_NeedsProto
#undef SymI_HasProto
#undef SymI_HasProto_redirect
#undef SymE_HasProto
#undef SymE_NeedsProto

#ifdef LEADING_UNDERSCORE
#define MAYBE_LEADING_UNDERSCORE_STR(s) ("_" s)
#else
#define MAYBE_LEADING_UNDERSCORE_STR(s) (s)
#endif

#define SymI_HasProto(vvv) { MAYBE_LEADING_UNDERSCORE_STR(#vvv), \
                    (void*)(&(vvv)) },
#define SymE_HasProto(vvv) { MAYBE_LEADING_UNDERSCORE_STR(#vvv), \
            (void*)DLL_IMPORT_DATA_REF(vvv) },

#define SymI_NeedsProto(vvv) SymI_HasProto(vvv)
#define SymE_NeedsProto(vvv) SymE_HasProto(vvv)

// SymI_HasProto_redirect allows us to redirect references to one symbol to
// another symbol.  See newCAF/newDynCAF for an example.
#define SymI_HasProto_redirect(vvv,xxx)   \
    { MAYBE_LEADING_UNDERSCORE_STR(#vvv), \
      (void*)(&(xxx)) },

static RtsSymbolVal rtsSyms[] = {
      RTS_SYMBOLS
      RTS_RET_SYMBOLS
      RTS_POSIX_ONLY_SYMBOLS
      RTS_MINGW_ONLY_SYMBOLS
      RTS_CYGWIN_ONLY_SYMBOLS
      RTS_DARWIN_ONLY_SYMBOLS
      RTS_LIBGCC_SYMBOLS
      RTS_LIBFFI_SYMBOLS
#if defined(darwin_HOST_OS) && defined(i386_HOST_ARCH)
      // dyld stub code contains references to this,
      // but it should never be called because we treat
      // lazy pointers as nonlazy.
      { "dyld_stub_binding_helper", (void*)0xDEADBEEF },
#endif
      { 0, 0 } /* sentinel */
};


/* -----------------------------------------------------------------------------
 * Insert symbols into hash tables, checking for duplicates.
 */

static void ghciInsertSymbolTable(
   pathchar* obj_name,
   HashTable *table,
   char* key,
   void *data,
   HsBool weak,
   ObjectCode *owner)
{
   RtsSymbolInfo *pinfo = lookupStrHashTable(table, key);
   if (!pinfo) /* new entry */
   {
      pinfo = stgMallocBytes(sizeof (*pinfo), "ghciInsertToSymbolTable");
      pinfo->value = data;
      pinfo->owner = owner;
      pinfo->weak = weak;
      insertStrHashTable(table, key, pinfo);
      return;
   } else if ((!pinfo->weak || pinfo->value) && weak) {
     return; /* duplicate weak symbol, throw it away */
   } else if (pinfo->weak) /* weak symbol is in the table */
   {
      /* override the weak definition with the non-weak one */
      pinfo->value = data;
      pinfo->owner = owner;
      pinfo->weak = HS_BOOL_FALSE;
      return;
   }
   debugBelch(
      "\n\n"
      "GHCi runtime linker: fatal error: I found a duplicate definition for symbol\n"
      "   %s\n"
      "whilst processing object file\n"
      "   %" PATH_FMT "\n"
      "This could be caused by:\n"
      "   * Loading two different object files which export the same symbol\n"
      "   * Specifying the same object file twice on the GHCi command line\n"
      "   * An incorrect `package.conf' entry, causing some object to be\n"
      "     loaded twice.\n"
      "GHCi cannot safely continue in this situation.  Exiting now.  Sorry.\n"
      "\n",
      (char*)key,
      obj_name
   );
   stg_exit(1);
}

static HsBool ghciLookupSymbolTable(HashTable *table,
    const char *key, void **result)
{
    RtsSymbolInfo *pinfo = lookupStrHashTable(table, key);
    if (!pinfo) {
        *result = NULL;
        return HS_BOOL_FALSE;
    }
    if (pinfo->weak)
        IF_DEBUG(linker, debugBelch("lookup: promoting %s\n", key));
    /* Once it's looked up, it can no longer be overridden */
    pinfo->weak = HS_BOOL_FALSE;

    *result = pinfo->value;
    return HS_BOOL_TRUE;
}

static void ghciRemoveSymbolTable(HashTable *table, const char *key,
    ObjectCode *owner)
{
    RtsSymbolInfo *pinfo = lookupStrHashTable(table, key);
    if (!pinfo || owner != pinfo->owner) return;
    removeStrHashTable(table, key, NULL);
    stgFree(pinfo);
}
/* -----------------------------------------------------------------------------
 * initialize the object linker
 */


static int linker_init_done = 0 ;

#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
static void *dl_prog_handle;
static regex_t re_invalid;
static regex_t re_realso;
#ifdef THREADED_RTS
static Mutex dl_mutex; // mutex to protect dlopen/dlerror critical section
#endif
#endif

void
initLinker( void )
{
    RtsSymbolVal *sym;
#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
    int compileResult;
#endif

    IF_DEBUG(linker, debugBelch("initLinker: start\n"));

    /* Make initLinker idempotent, so we can call it
       before every relevant operation; that means we
       don't need to initialise the linker separately */
    if (linker_init_done == 1) {
        IF_DEBUG(linker, debugBelch("initLinker: idempotent return\n"));
        return;
    } else {
        linker_init_done = 1;
    }

    objects = NULL;
    unloaded_objects = NULL;

#if defined(THREADED_RTS) && (defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO))
    initMutex(&dl_mutex);
#endif
    symhash = allocStrHashTable();

    /* populate the symbol table with stuff from the RTS */
    for (sym = rtsSyms; sym->lbl != NULL; sym++) {
        ghciInsertSymbolTable(WSTR("(GHCi built-in symbols)"),
                               symhash, sym->lbl, sym->addr, HS_BOOL_FALSE, NULL);
        IF_DEBUG(linker, debugBelch("initLinker: inserting rts symbol %s, %p\n", sym->lbl, sym->addr));
    }
#   if defined(OBJFORMAT_MACHO) && defined(powerpc_HOST_ARCH)
    machoInitSymbolsWithoutUnderscore();
#   endif
    /* GCC defines a special symbol __dso_handle which is resolved to NULL if
       referenced from a statically linked module. We need to mimic this, but
       we cannot use NULL because we use it to mean nonexistent symbols. So we
       use an arbitrary (hopefully unique) address here.
    */
    ghciInsertSymbolTable(WSTR("(GHCi special symbols)"),
        symhash, "__dso_handle", (void *)0x12345687, HS_BOOL_FALSE, NULL);

#   if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
#   if defined(RTLD_DEFAULT)
    dl_prog_handle = RTLD_DEFAULT;
#   else
    dl_prog_handle = dlopen(NULL, RTLD_LAZY);
#   endif /* RTLD_DEFAULT */

    compileResult = regcomp(&re_invalid,
           "(([^ \t()])+\\.so([^ \t:()])*):([ \t])*(invalid ELF header|file too short)",
           REG_EXTENDED);
    if (compileResult != 0) {
        barf("Compiling re_invalid failed");
    }
    compileResult = regcomp(&re_realso,
           "(GROUP|INPUT) *\\( *([^ )]+)",
           REG_EXTENDED);
    if (compileResult != 0) {
        barf("Compiling re_realso failed");
    }
#   endif

#if !defined(ALWAYS_PIC) && defined(x86_64_HOST_ARCH)
    if (RtsFlags.MiscFlags.linkerMemBase != 0) {
        // User-override for mmap_32bit_base
        mmap_32bit_base = (void*)RtsFlags.MiscFlags.linkerMemBase;
    }
#endif

#if defined(mingw32_HOST_OS)
    /*
     * These two libraries cause problems when added to the static link,
     * but are necessary for resolving symbols in GHCi, hence we load
     * them manually here.
     */
    addDLL(WSTR("msvcrt"));
    addDLL(WSTR("kernel32"));
#endif

    IF_DEBUG(linker, debugBelch("initLinker: done\n"));
    return;
}

void
exitLinker( void ) {
#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
   if (linker_init_done == 1) {
      regfree(&re_invalid);
      regfree(&re_realso);
#ifdef THREADED_RTS
      closeMutex(&dl_mutex);
#endif
   }
#endif
}

/* -----------------------------------------------------------------------------
 *                  Loading DLL or .so dynamic libraries
 * -----------------------------------------------------------------------------
 *
 * Add a DLL from which symbols may be found.  In the ELF case, just
 * do RTLD_GLOBAL-style add, so no further messing around needs to
 * happen in order that symbols in the loaded .so are findable --
 * lookupSymbol() will subsequently see them by dlsym on the program's
 * dl-handle.  Returns NULL if success, otherwise ptr to an err msg.
 *
 * In the PEi386 case, open the DLLs and put handles to them in a
 * linked list.  When looking for a symbol, try all handles in the
 * list.  This means that we need to load even DLLs that are guaranteed
 * to be in the ghc.exe image already, just so we can get a handle
 * to give to loadSymbol, so that we can find the symbols.  For such
 * libraries, the LoadLibrary call should be a no-op except for returning
 * the handle.
 *
 */

#if defined(OBJFORMAT_PEi386)
/* A record for storing handles into DLLs. */

typedef
   struct _OpenedDLL {
      pathchar*          name;
      struct _OpenedDLL* next;
      HINSTANCE instance;
   }
   OpenedDLL;

/* A list thereof. */
static OpenedDLL* opened_dlls = NULL;
#endif

#  if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)

/* Suppose in ghci we load a temporary SO for a module containing
       f = 1
   and then modify the module, recompile, and load another temporary
   SO with
       f = 2
   Then as we don't unload the first SO, dlsym will find the
       f = 1
   symbol whereas we want the
       f = 2
   symbol. We therefore need to keep our own SO handle list, and
   try SOs in the right order. */

typedef
   struct _OpenedSO {
      struct _OpenedSO* next;
      void *handle;
   }
   OpenedSO;

/* A list thereof. */
static OpenedSO* openedSOs = NULL;

static const char *
internal_dlopen(const char *dll_name)
{
   OpenedSO* o_so;
   void *hdl;
   const char *errmsg;
   char *errmsg_copy;

   // omitted: RTLD_NOW
   // see http://www.haskell.org/pipermail/cvs-ghc/2007-September/038570.html
   IF_DEBUG(linker,
      debugBelch("internal_dlopen: dll_name = '%s'\n", dll_name));

   //-------------- Begin critical section ------------------
   // This critical section is necessary because dlerror() is not
   // required to be reentrant (see POSIX -- IEEE Std 1003.1-2008)
   // Also, the error message returned must be copied to preserve it
   // (see POSIX also)

   ACQUIRE_LOCK(&dl_mutex);
   hdl = dlopen(dll_name, RTLD_LAZY | RTLD_GLOBAL);

   errmsg = NULL;
   if (hdl == NULL) {
      /* dlopen failed; return a ptr to the error msg. */
      errmsg = dlerror();
      if (errmsg == NULL) errmsg = "addDLL: unknown error";
      errmsg_copy = stgMallocBytes(strlen(errmsg)+1, "addDLL");
      strcpy(errmsg_copy, errmsg);
      errmsg = errmsg_copy;
   }
   o_so = stgMallocBytes(sizeof(OpenedSO), "addDLL");
   o_so->handle = hdl;
   o_so->next   = openedSOs;
   openedSOs    = o_so;

   RELEASE_LOCK(&dl_mutex);
   //--------------- End critical section -------------------

   return errmsg;
}

static void *
internal_dlsym(void *hdl, const char *symbol) {
    OpenedSO* o_so;
    void *v;

    // We acquire dl_mutex as concurrent dl* calls may alter dlerror
    ACQUIRE_LOCK(&dl_mutex);
    dlerror();
    for (o_so = openedSOs; o_so != NULL; o_so = o_so->next) {
        v = dlsym(o_so->handle, symbol);
        if (dlerror() == NULL) {
            RELEASE_LOCK(&dl_mutex);
            return v;
        }
    }
    v = dlsym(hdl, symbol);
    RELEASE_LOCK(&dl_mutex);
    return v;
}
#  endif

const char *
addDLL( pathchar *dll_name )
{
#  if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
   /* ------------------- ELF DLL loader ------------------- */

#define NMATCH 5
   regmatch_t match[NMATCH];
   const char *errmsg;
   FILE* fp;
   size_t match_length;
#define MAXLINE 1000
   char line[MAXLINE];
   int result;

   initLinker();

   IF_DEBUG(linker, debugBelch("addDLL: dll_name = '%s'\n", dll_name));
   errmsg = internal_dlopen(dll_name);

   if (errmsg == NULL) {
      return NULL;
   }

   // GHC Trac ticket #2615
   // On some systems (e.g., Gentoo Linux) dynamic files (e.g. libc.so)
   // contain linker scripts rather than ELF-format object code. This
   // code handles the situation by recognizing the real object code
   // file name given in the linker script.
   //
   // If an "invalid ELF header" error occurs, it is assumed that the
   // .so file contains a linker script instead of ELF object code.
   // In this case, the code looks for the GROUP ( ... ) linker
   // directive. If one is found, the first file name inside the
   // parentheses is treated as the name of a dynamic library and the
   // code attempts to dlopen that file. If this is also unsuccessful,
   // an error message is returned.

   // see if the error message is due to an invalid ELF header
   IF_DEBUG(linker, debugBelch("errmsg = '%s'\n", errmsg));
   result = regexec(&re_invalid, errmsg, (size_t) NMATCH, match, 0);
   IF_DEBUG(linker, debugBelch("result = %i\n", result));
   if (result == 0) {
      // success -- try to read the named file as a linker script
      match_length = (size_t) stg_min((match[1].rm_eo - match[1].rm_so),
                                 MAXLINE-1);
      strncpy(line, (errmsg+(match[1].rm_so)),match_length);
      line[match_length] = '\0'; // make sure string is null-terminated
      IF_DEBUG(linker, debugBelch ("file name = '%s'\n", line));
      if ((fp = fopen(line, "r")) == NULL) {
         return errmsg; // return original error if open fails
      }
      // try to find a GROUP or INPUT ( ... ) command
      while (fgets(line, MAXLINE, fp) != NULL) {
         IF_DEBUG(linker, debugBelch("input line = %s", line));
         if (regexec(&re_realso, line, (size_t) NMATCH, match, 0) == 0) {
            // success -- try to dlopen the first named file
            IF_DEBUG(linker, debugBelch("match%s\n",""));
            line[match[2].rm_eo] = '\0';
            errmsg = internal_dlopen(line+match[2].rm_so);
            break;
         }
         // if control reaches here, no GROUP or INPUT ( ... ) directive
         // was found and the original error message is returned to the
         // caller
      }
      fclose(fp);
   }
   return errmsg;

#  elif defined(OBJFORMAT_PEi386)
   /* ------------------- Win32 DLL loader ------------------- */

   pathchar*      buf;
   OpenedDLL* o_dll;
   HINSTANCE  instance;

   initLinker();

   /* debugBelch("\naddDLL; dll_name = `%s'\n", dll_name); */

   /* See if we've already got it, and ignore if so. */
   for (o_dll = opened_dlls; o_dll != NULL; o_dll = o_dll->next) {
      if (0 == pathcmp(o_dll->name, dll_name))
         return NULL;
   }

   /* The file name has no suffix (yet) so that we can try
      both foo.dll and foo.drv

      The documentation for LoadLibrary says:
        If no file name extension is specified in the lpFileName
        parameter, the default library extension .dll is
        appended. However, the file name string can include a trailing
        point character (.) to indicate that the module name has no
        extension. */

   buf = stgMallocBytes((pathlen(dll_name) + 10) * sizeof(wchar_t), "addDLL");
   swprintf(buf, L"%s.DLL", dll_name);
   instance = LoadLibraryW(buf);
   if (instance == NULL) {
       if (GetLastError() != ERROR_MOD_NOT_FOUND) goto error;
       // KAA: allow loading of drivers (like winspool.drv)
       swprintf(buf, L"%s.DRV", dll_name);
       instance = LoadLibraryW(buf);
       if (instance == NULL) {
           if (GetLastError() != ERROR_MOD_NOT_FOUND) goto error;
           // #1883: allow loading of unix-style libfoo.dll DLLs
           swprintf(buf, L"lib%s.DLL", dll_name);
           instance = LoadLibraryW(buf);
           if (instance == NULL) {
               goto error;
           }
       }
   }
   stgFree(buf);

   /* Add this DLL to the list of DLLs in which to search for symbols. */
   o_dll = stgMallocBytes( sizeof(OpenedDLL), "addDLL" );
   o_dll->name     = pathdup(dll_name);
   o_dll->instance = instance;
   o_dll->next     = opened_dlls;
   opened_dlls     = o_dll;

   return NULL;

error:
   stgFree(buf);
   sysErrorBelch("%" PATH_FMT, dll_name);

   /* LoadLibrary failed; return a ptr to the error msg. */
   return "addDLL: could not load DLL";

#  else
   barf("addDLL: not implemented on this platform");
#  endif
}

/* -----------------------------------------------------------------------------
 * insert a symbol in the hash table
 */
void
insertSymbol(pathchar* obj_name, char* key, void* data)
{
  ghciInsertSymbolTable(obj_name, symhash, key, data, HS_BOOL_FALSE, NULL);
}

/* -----------------------------------------------------------------------------
 * lookup a symbol in the hash table
 */
void *
lookupSymbol( char *lbl )
{
    void *val;
    IF_DEBUG(linker, debugBelch("lookupSymbol: looking up %s\n", lbl));
    initLinker() ;
    ASSERT(symhash != NULL);

    if (!ghciLookupSymbolTable(symhash, lbl, &val)) {
        IF_DEBUG(linker, debugBelch("lookupSymbol: symbol not found\n"));
#       if defined(OBJFORMAT_ELF)
        return internal_dlsym(dl_prog_handle, lbl);
#       elif defined(OBJFORMAT_MACHO)
#       if HAVE_DLFCN_H
        /* On OS X 10.3 and later, we use dlsym instead of the old legacy
           interface.

           HACK: On OS X, all symbols are prefixed with an underscore.
                 However, dlsym wants us to omit the leading underscore from the
                 symbol name -- the dlsym routine puts it back on before searching
                 for the symbol. For now, we simply strip it off here (and ONLY
                 here).
        */
        IF_DEBUG(linker, debugBelch("lookupSymbol: looking up %s with dlsym\n", lbl));
        ASSERT(lbl[0] == '_');
        return internal_dlsym(dl_prog_handle, lbl + 1);
#       else
        if (NSIsSymbolNameDefined(lbl)) {
            NSSymbol symbol = NSLookupAndBindSymbol(lbl);
            return NSAddressOfSymbol(symbol);
        } else {
            return NULL;
        }
#       endif /* HAVE_DLFCN_H */
#       elif defined(OBJFORMAT_PEi386)
        void* sym;

        sym = lookupSymbolInDLLs((unsigned char*)lbl);
        if (sym != NULL) { return sym; };

        // Also try looking up the symbol without the @N suffix.  Some
        // DLLs have the suffixes on their symbols, some don't.
        zapTrailingAtSign ( (unsigned char*)lbl );
        sym = lookupSymbolInDLLs((unsigned char*)lbl);
        if (sym != NULL) { return sym; };
        return NULL;

#       else
        ASSERT(2+2 == 5);
        return NULL;
#       endif
    } else {
        IF_DEBUG(linker, debugBelch("lookupSymbol: value of %s is %p\n", lbl, val));
        return val;
    }
}

/* -----------------------------------------------------------------------------
   Create a StablePtr for a foreign export.  This is normally called by
   a C function with __attribute__((constructor)), which is generated
   by GHC and linked into the module.

   If the object code is being loaded dynamically, then we remember
   which StablePtrs were allocated by the constructors and free them
   again in unloadObj().
   -------------------------------------------------------------------------- */

static ObjectCode *loading_obj = NULL;

StgStablePtr foreignExportStablePtr (StgPtr p)
{
    ForeignExportStablePtr *fe_sptr;
    StgStablePtr *sptr;

    sptr = getStablePtr(p);

    if (loading_obj != NULL) {
        fe_sptr = stgMallocBytes(sizeof(ForeignExportStablePtr),
                                 "foreignExportStablePtr");
        fe_sptr->stable_ptr = sptr;
        fe_sptr->next = loading_obj->stable_ptrs;
        loading_obj->stable_ptrs = fe_sptr;
    }

    return sptr;
}


/* -----------------------------------------------------------------------------
 * Debugging aid: look in GHCi's object symbol tables for symbols
 * within DELTA bytes of the specified address, and show their names.
 */
#ifdef DEBUG
void ghci_enquire ( char* addr );

void ghci_enquire ( char* addr )
{
   int   i;
   char* sym;
   char* a;
   const int DELTA = 64;
   ObjectCode* oc;

   initLinker();

   for (oc = objects; oc; oc = oc->next) {
      for (i = 0; i < oc->n_symbols; i++) {
         sym = oc->symbols[i];
         if (sym == NULL) continue;
         a = NULL;
         if (a == NULL) {
            ghciLookupSymbolTable(symhash, sym, (void **)&a);
         }
         if (a == NULL) {
             // debugBelch("ghci_enquire: can't find %s\n", sym);
         }
         else if (addr-DELTA <= a && a <= addr+DELTA) {
            debugBelch("%p + %3d  ==  `%s'\n", addr, (int)(a - addr), sym);
         }
      }
   }
}
#endif

#ifdef USE_MMAP
#define ROUND_UP(x,size) ((x + size - 1) & ~(size - 1))

static void *
mmapForLinker (size_t bytes, nat flags, int fd)
{
   void *map_addr = NULL;
   void *result;
   int pagesize, size;
   static nat fixed = 0;

   IF_DEBUG(linker, debugBelch("mmapForLinker: start\n"));
   pagesize = getpagesize();
   size = ROUND_UP(bytes, pagesize);

#if !defined(ALWAYS_PIC) && defined(x86_64_HOST_ARCH)
mmap_again:

   if (mmap_32bit_base != 0) {
       map_addr = mmap_32bit_base;
   }
#endif

   IF_DEBUG(linker, debugBelch("mmapForLinker: \tprotection %#0x\n", PROT_EXEC | PROT_READ | PROT_WRITE));
   IF_DEBUG(linker, debugBelch("mmapForLinker: \tflags      %#0x\n", MAP_PRIVATE | TRY_MAP_32BIT | fixed | flags));
   result = mmap(map_addr, size, PROT_EXEC|PROT_READ|PROT_WRITE,
                    MAP_PRIVATE|TRY_MAP_32BIT|fixed|flags, fd, 0);

   if (result == MAP_FAILED) {
       sysErrorBelch("mmap %" FMT_Word " bytes at %p",(W_)size,map_addr);
       errorBelch("Try specifying an address with +RTS -xm<addr> -RTS");
       stg_exit(EXIT_FAILURE);
   }

#if !defined(ALWAYS_PIC) && defined(x86_64_HOST_ARCH)
   if (mmap_32bit_base != 0) {
       if (result == map_addr) {
           mmap_32bit_base = (StgWord8*)map_addr + size;
       } else {
           if ((W_)result > 0x80000000) {
               // oops, we were given memory over 2Gb
#if defined(freebsd_HOST_OS)  || defined(kfreebsdgnu_HOST_OS) || defined(dragonfly_HOST_OS)
               // Some platforms require MAP_FIXED.  This is normally
               // a bad idea, because MAP_FIXED will overwrite
               // existing mappings.
               munmap(result,size);
               fixed = MAP_FIXED;
               goto mmap_again;
#else
               barf("loadObj: failed to mmap() memory below 2Gb; asked for %lu bytes at %p.  Try specifying an address with +RTS -xm<addr> -RTS", size, map_addr, result);
#endif
           } else {
               // hmm, we were given memory somewhere else, but it's
               // still under 2Gb so we can use it.  Next time, ask
               // for memory right after the place we just got some
               mmap_32bit_base = (StgWord8*)result + size;
           }
       }
   } else {
       if ((W_)result > 0x80000000) {
           // oops, we were given memory over 2Gb
           // ... try allocating memory somewhere else?;
           debugTrace(DEBUG_linker,"MAP_32BIT didn't work; gave us %lu bytes at 0x%p", bytes, result);
           munmap(result, size);

           // Set a base address and try again... (guess: 1Gb)
           mmap_32bit_base = (void*)0x40000000;
           goto mmap_again;
       }
   }
#endif

   IF_DEBUG(linker, debugBelch("mmapForLinker: mapped %" FMT_Word " bytes starting at %p\n", (W_)size, result));
   IF_DEBUG(linker, debugBelch("mmapForLinker: done\n"));
   return result;
}
#endif // USE_MMAP


void freeObjectCode (ObjectCode *oc)
{
#ifdef USE_MMAP
    int pagesize, size, r;

    pagesize = getpagesize();
    size = ROUND_UP(oc->fileSize, pagesize);

    r = munmap(oc->image, size);
    if (r == -1) {
        sysErrorBelch("munmap");
    }

#if defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH) || defined(arm_HOST_ARCH)
#if !defined(x86_64_HOST_ARCH) || !defined(mingw32_HOST_OS)
    if (!USE_CONTIGUOUS_MMAP)
    {
        munmap(oc->symbol_extras,
               ROUND_UP(sizeof(SymbolExtra) * oc->n_symbol_extras, pagesize));
    }
#endif
#endif

#else

    stgFree(oc->image);

#if defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH) || defined(arm_HOST_ARCH)
#if !defined(x86_64_HOST_ARCH) || !defined(mingw32_HOST_OS)
    stgFree(oc->symbol_extras);
#endif
#endif

#endif

    stgFree(oc->fileName);
    stgFree(oc->archiveMemberName);
    stgFree(oc);
}


static ObjectCode*
mkOc( pathchar *path, char *image, int imageSize,
      char *archiveMemberName
#ifndef USE_MMAP
#ifdef darwin_HOST_OS
    , int misalignment
#endif
#endif
    ) {
   ObjectCode* oc;

   IF_DEBUG(linker, debugBelch("mkOc: start\n"));
   oc = stgMallocBytes(sizeof(ObjectCode), "loadArchive(oc)");

#  if defined(OBJFORMAT_ELF)
   oc->formatName = "ELF";
#  elif defined(OBJFORMAT_PEi386)
   oc->formatName = "PEi386";
#  elif defined(OBJFORMAT_MACHO)
   oc->formatName = "Mach-O";
#  else
   stgFree(oc);
   barf("loadObj: not implemented on this platform");
#  endif

   oc->image = image;
   oc->fileName = pathdup(path);

   if (archiveMemberName) {
       oc->archiveMemberName = stgMallocBytes( strlen(archiveMemberName)+1, "loadObj" );
       strcpy(oc->archiveMemberName, archiveMemberName);
   }
   else {
       oc->archiveMemberName = NULL;
   }

   oc->fileSize          = imageSize;
   oc->symbols           = NULL;
   oc->sections          = NULL;
   oc->proddables        = NULL;
   oc->stable_ptrs       = NULL;

#ifndef USE_MMAP
#ifdef darwin_HOST_OS
   oc->misalignment = misalignment;
#endif
#endif

   /* chain it onto the list of objects */
   oc->next              = objects;
   objects               = oc;

   IF_DEBUG(linker, debugBelch("mkOc: done\n"));
   return oc;
}

HsInt
loadArchive( pathchar *path )
{
    ObjectCode* oc;
    char *image;
    int memberSize;
    FILE *f;
    int n;
    size_t thisFileNameSize;
    char *fileName;
    size_t fileNameSize;
    int isObject, isGnuIndex;
    char tmp[20];
    char *gnuFileIndex;
    int gnuFileIndexSize;
#if defined(darwin_HOST_OS)
    int i;
    uint32_t nfat_arch, nfat_offset, cputype, cpusubtype;
#if defined(i386_HOST_ARCH)
    const uint32_t mycputype = CPU_TYPE_X86;
    const uint32_t mycpusubtype = CPU_SUBTYPE_X86_ALL;
#elif defined(x86_64_HOST_ARCH)
    const uint32_t mycputype = CPU_TYPE_X86_64;
    const uint32_t mycpusubtype = CPU_SUBTYPE_X86_64_ALL;
#elif defined(powerpc_HOST_ARCH)
    const uint32_t mycputype = CPU_TYPE_POWERPC;
    const uint32_t mycpusubtype = CPU_SUBTYPE_POWERPC_ALL;
#elif defined(powerpc64_HOST_ARCH)
    const uint32_t mycputype = CPU_TYPE_POWERPC64;
    const uint32_t mycpusubtype = CPU_SUBTYPE_POWERPC_ALL;
#else
#error Unknown Darwin architecture
#endif
#if !defined(USE_MMAP)
    int misalignment;
#endif
#endif

    IF_DEBUG(linker, debugBelch("loadArchive: start\n"));
    IF_DEBUG(linker, debugBelch("loadArchive: Loading archive `%" PATH_FMT" '\n", path));

    gnuFileIndex = NULL;
    gnuFileIndexSize = 0;

    fileNameSize = 32;
    fileName = stgMallocBytes(fileNameSize, "loadArchive(fileName)");

    f = pathopen(path, WSTR("rb"));
    if (!f)
        barf("loadObj: can't read `%s'", path);

    /* Check if this is an archive by looking for the magic "!<arch>\n"
     * string.  Usually, if this fails, we barf and quit.  On Darwin however,
     * we may have a fat archive, which contains archives for more than
     * one architecture.  Fat archives start with the magic number 0xcafebabe,
     * always stored big endian.  If we find a fat_header, we scan through
     * the fat_arch structs, searching through for one for our host
     * architecture.  If a matching struct is found, we read the offset
     * of our archive data (nfat_offset) and seek forward nfat_offset bytes
     * from the start of the file.
     *
     * A subtlety is that all of the members of the fat_header and fat_arch
     * structs are stored big endian, so we need to call byte order
     * conversion functions.
     *
     * If we find the appropriate architecture in a fat archive, we gobble
     * its magic "!<arch>\n" string and continue processing just as if
     * we had a single architecture archive.
     */

    n = fread ( tmp, 1, 8, f );
    if (n != 8)
        barf("loadArchive: Failed reading header from `%s'", path);
    if (strncmp(tmp, "!<arch>\n", 8) != 0) {

#if defined(darwin_HOST_OS)
        /* Not a standard archive, look for a fat archive magic number: */
        if (ntohl(*(uint32_t *)tmp) == FAT_MAGIC) {
            nfat_arch = ntohl(*(uint32_t *)(tmp + 4));
            IF_DEBUG(linker, debugBelch("loadArchive: found a fat archive containing %d architectures\n", nfat_arch));
            nfat_offset = 0;

            for (i = 0; i < (int)nfat_arch; i++) {
                /* search for the right arch */
                n = fread( tmp, 1, 20, f );
                if (n != 8)
                    barf("loadArchive: Failed reading arch from `%s'", path);
                cputype = ntohl(*(uint32_t *)tmp);
                cpusubtype = ntohl(*(uint32_t *)(tmp + 4));

                if (cputype == mycputype && cpusubtype == mycpusubtype) {
                    IF_DEBUG(linker, debugBelch("loadArchive: found my archive in a fat archive\n"));
                    nfat_offset = ntohl(*(uint32_t *)(tmp + 8));
                    break;
                }
            }

            if (nfat_offset == 0) {
               barf ("loadArchive: searched %d architectures, but no host arch found", (int)nfat_arch);
            }
            else {
                n = fseek( f, nfat_offset, SEEK_SET );
                if (n != 0)
                    barf("loadArchive: Failed to seek to arch in `%s'", path);
                n = fread ( tmp, 1, 8, f );
                if (n != 8)
                    barf("loadArchive: Failed reading header from `%s'", path);
                if (strncmp(tmp, "!<arch>\n", 8) != 0) {
                    barf("loadArchive: couldn't find archive in `%s' at offset %d", path, nfat_offset);
                }
            }
        }
        else {
            barf("loadArchive: Neither an archive, nor a fat archive: `%s'", path);
        }

#else
        barf("loadArchive: Not an archive: `%s'", path);
#endif
    }

    IF_DEBUG(linker, debugBelch("loadArchive: loading archive contents\n"));

    while(1) {
        n = fread ( fileName, 1, 16, f );
        if (n != 16) {
            if (feof(f)) {
                IF_DEBUG(linker, debugBelch("loadArchive: EOF while reading from '%" PATH_FMT "'\n", path));
                break;
            }
            else {
                barf("loadArchive: Failed reading file name from `%s'", path);
            }
        }

#if defined(darwin_HOST_OS)
        if (strncmp(fileName, "!<arch>\n", 8) == 0) {
            IF_DEBUG(linker, debugBelch("loadArchive: found the start of another archive, breaking\n"));
            break;
        }
#endif

        n = fread ( tmp, 1, 12, f );
        if (n != 12)
            barf("loadArchive: Failed reading mod time from `%s'", path);
        n = fread ( tmp, 1, 6, f );
        if (n != 6)
            barf("loadArchive: Failed reading owner from `%s'", path);
        n = fread ( tmp, 1, 6, f );
        if (n != 6)
            barf("loadArchive: Failed reading group from `%s'", path);
        n = fread ( tmp, 1, 8, f );
        if (n != 8)
            barf("loadArchive: Failed reading mode from `%s'", path);
        n = fread ( tmp, 1, 10, f );
        if (n != 10)
            barf("loadArchive: Failed reading size from `%s'", path);
        tmp[10] = '\0';
        for (n = 0; isdigit(tmp[n]); n++);
        tmp[n] = '\0';
        memberSize = atoi(tmp);

        IF_DEBUG(linker, debugBelch("loadArchive: size of this archive member is %d\n", memberSize));
        n = fread ( tmp, 1, 2, f );
        if (n != 2)
            barf("loadArchive: Failed reading magic from `%s'", path);
        if (strncmp(tmp, "\x60\x0A", 2) != 0)
            barf("loadArchive: Failed reading magic from `%s' at %ld. Got %c%c",
                 path, ftell(f), tmp[0], tmp[1]);

        isGnuIndex = 0;
        /* Check for BSD-variant large filenames */
        if (0 == strncmp(fileName, "#1/", 3)) {
            fileName[16] = '\0';
            if (isdigit(fileName[3])) {
                for (n = 4; isdigit(fileName[n]); n++);
                fileName[n] = '\0';
                thisFileNameSize = atoi(fileName + 3);
                memberSize -= thisFileNameSize;
                if (thisFileNameSize >= fileNameSize) {
                    /* Double it to avoid potentially continually
                       increasing it by 1 */
                    fileNameSize = thisFileNameSize * 2;
                    fileName = stgReallocBytes(fileName, fileNameSize, "loadArchive(fileName)");
                }
                n = fread ( fileName, 1, thisFileNameSize, f );
                if (n != (int)thisFileNameSize) {
                    barf("loadArchive: Failed reading filename from `%s'",
                         path);
                }
                fileName[thisFileNameSize] = 0;

                /* On OS X at least, thisFileNameSize is the size of the
                   fileName field, not the length of the fileName
                   itself. */
                thisFileNameSize = strlen(fileName);
            }
            else {
                barf("loadArchive: BSD-variant filename size not found while reading filename from `%s'", path);
            }
        }
        /* Check for GNU file index file */
        else if (0 == strncmp(fileName, "//", 2)) {
            fileName[0] = '\0';
            thisFileNameSize = 0;
            isGnuIndex = 1;
        }
        /* Check for a file in the GNU file index */
        else if (fileName[0] == '/') {
            if (isdigit(fileName[1])) {
                int i;

                for (n = 2; isdigit(fileName[n]); n++);
                fileName[n] = '\0';
                n = atoi(fileName + 1);

                if (gnuFileIndex == NULL) {
                    barf("loadArchive: GNU-variant filename without an index while reading from `%s'", path);
                }
                if (n < 0 || n > gnuFileIndexSize) {
                    barf("loadArchive: GNU-variant filename offset %d out of range [0..%d] while reading filename from `%s'", n, gnuFileIndexSize, path);
                }
                if (n != 0 && gnuFileIndex[n - 1] != '\n') {
                    barf("loadArchive: GNU-variant filename offset %d invalid (range [0..%d]) while reading filename from `%s'", n, gnuFileIndexSize, path);
                }
                for (i = n; gnuFileIndex[i] != '/'; i++);
                thisFileNameSize = i - n;
                if (thisFileNameSize >= fileNameSize) {
                    /* Double it to avoid potentially continually
                       increasing it by 1 */
                    fileNameSize = thisFileNameSize * 2;
                    fileName = stgReallocBytes(fileName, fileNameSize, "loadArchive(fileName)");
                }
                memcpy(fileName, gnuFileIndex + n, thisFileNameSize);
                fileName[thisFileNameSize] = '\0';
            }
            else if (fileName[1] == ' ') {
                fileName[0] = '\0';
                thisFileNameSize = 0;
            }
            else {
                barf("loadArchive: GNU-variant filename offset not found while reading filename from `%s'", path);
            }
        }
        /* Finally, the case where the filename field actually contains
           the filename */
        else {
            /* GNU ar terminates filenames with a '/', this allowing
               spaces in filenames. So first look to see if there is a
               terminating '/'. */
            for (thisFileNameSize = 0;
                 thisFileNameSize < 16;
                 thisFileNameSize++) {
                if (fileName[thisFileNameSize] == '/') {
                    fileName[thisFileNameSize] = '\0';
                    break;
                }
            }
            /* If we didn't find a '/', then a space teminates the
               filename. Note that if we don't find one, then
               thisFileNameSize ends up as 16, and we already have the
               '\0' at the end. */
            if (thisFileNameSize == 16) {
                for (thisFileNameSize = 0;
                     thisFileNameSize < 16;
                     thisFileNameSize++) {
                    if (fileName[thisFileNameSize] == ' ') {
                        fileName[thisFileNameSize] = '\0';
                        break;
                    }
                }
            }
        }

        IF_DEBUG(linker,
                 debugBelch("loadArchive: Found member file `%s'\n", fileName));

        isObject = thisFileNameSize >= 2
                && fileName[thisFileNameSize - 2] == '.'
                && fileName[thisFileNameSize - 1] == 'o';

        IF_DEBUG(linker, debugBelch("loadArchive: \tthisFileNameSize = %d\n", (int)thisFileNameSize));
        IF_DEBUG(linker, debugBelch("loadArchive: \tisObject = %d\n", isObject));

        if (isObject) {
            char *archiveMemberName;

            IF_DEBUG(linker, debugBelch("loadArchive: Member is an object file...loading...\n"));

            /* We can't mmap from the archive directly, as object
               files need to be 8-byte aligned but files in .ar
               archives are 2-byte aligned. When possible we use mmap
               to get some anonymous memory, as on 64-bit platforms if
               we use malloc then we can be given memory above 2^32.
               In the mmap case we're probably wasting lots of space;
               we could do better. */
#if defined(USE_MMAP)
            image = mmapForLinker(memberSize, MAP_ANONYMOUS, -1);
#elif defined(mingw32_HOST_OS)
        // TODO: We would like to use allocateExec here, but allocateExec
        //       cannot currently allocate blocks large enough.
            {
                int offset;
#if defined(x86_64_HOST_ARCH)
                /* We get back 8-byte aligned memory (is that guaranteed?), but
                   the offsets to the sections within the file are all 4 mod 8
                   (is that guaranteed?). We therefore need to offset the image
                   by 4, so that all the pointers are 8-byte aligned, so that
                   pointer tagging works. */
                offset = 4;
#else
                offset = 0;
#endif
                image = VirtualAlloc(NULL, memberSize + offset,
                                     MEM_RESERVE | MEM_COMMIT,
                                     PAGE_EXECUTE_READWRITE);
                image += offset;
            }
#elif defined(darwin_HOST_OS)
            /* See loadObj() */
            misalignment = machoGetMisalignment(f);
            image = stgMallocBytes(memberSize + misalignment, "loadArchive(image)");
            image += misalignment;
#else
            image = stgMallocBytes(memberSize, "loadArchive(image)");
#endif
            n = fread ( image, 1, memberSize, f );
            if (n != memberSize) {
                barf("loadArchive: error whilst reading `%s'", path);
            }

            archiveMemberName = stgMallocBytes(pathlen(path) + thisFileNameSize + 3,
                                               "loadArchive(file)");
            sprintf(archiveMemberName, "%" PATH_FMT "(%.*s)",
                    path, (int)thisFileNameSize, fileName);

            oc = mkOc(path, image, memberSize, archiveMemberName
#ifndef USE_MMAP
#ifdef darwin_HOST_OS
                     , misalignment
#endif
#endif
                     );

            stgFree(archiveMemberName);

            if (0 == loadOc(oc)) {
                stgFree(fileName);
                return 0;
            }
        }
        else if (isGnuIndex) {
            if (gnuFileIndex != NULL) {
                barf("loadArchive: GNU-variant index found, but already have an index, while reading filename from `%s'", path);
            }
            IF_DEBUG(linker, debugBelch("loadArchive: Found GNU-variant file index\n"));
#ifdef USE_MMAP
            gnuFileIndex = mmapForLinker(memberSize + 1, MAP_ANONYMOUS, -1);
#else
            gnuFileIndex = stgMallocBytes(memberSize + 1, "loadArchive(image)");
#endif
            n = fread ( gnuFileIndex, 1, memberSize, f );
            if (n != memberSize) {
                barf("loadArchive: error whilst reading `%s'", path);
            }
            gnuFileIndex[memberSize] = '/';
            gnuFileIndexSize = memberSize;
        }
        else {
            IF_DEBUG(linker, debugBelch("loadArchive: '%s' does not appear to be an object file\n", fileName));
            n = fseek(f, memberSize, SEEK_CUR);
            if (n != 0)
                barf("loadArchive: error whilst seeking by %d in `%s'",
                     memberSize, path);
        }

        /* .ar files are 2-byte aligned */
        if (memberSize % 2) {
            IF_DEBUG(linker, debugBelch("loadArchive: trying to read one pad byte\n"));
            n = fread ( tmp, 1, 1, f );
            if (n != 1) {
                if (feof(f)) {
                    IF_DEBUG(linker, debugBelch("loadArchive: found EOF while reading one pad byte\n"));
                    break;
                }
                else {
                    barf("loadArchive: Failed reading padding from `%s'", path);
                }
            }
            IF_DEBUG(linker, debugBelch("loadArchive: successfully read one pad byte\n"));
        }
        IF_DEBUG(linker, debugBelch("loadArchive: reached end of archive loading while loop\n"));
    }

    fclose(f);

    stgFree(fileName);
    if (gnuFileIndex != NULL) {
#ifdef USE_MMAP
        munmap(gnuFileIndex, gnuFileIndexSize + 1);
#else
        stgFree(gnuFileIndex);
#endif
    }

    IF_DEBUG(linker, debugBelch("loadArchive: done\n"));
    return 1;
}

/* -----------------------------------------------------------------------------
 * Load an obj (populate the global symbol table, but don't resolve yet)
 *
 * Returns: 1 if ok, 0 on error.
 */
HsInt
loadObj( pathchar *path )
{
   ObjectCode* oc;
   char *image;
   int fileSize;
   struct_stat st;
   int r;
#ifdef USE_MMAP
   int fd;
#else
   FILE *f;
#  if defined(darwin_HOST_OS)
   int misalignment;
#  endif
#endif
   IF_DEBUG(linker, debugBelch("loadObj %" PATH_FMT "\n", path));

   initLinker();

   /* debugBelch("loadObj %s\n", path ); */

   /* Check that we haven't already loaded this object.
      Ignore requests to load multiple times */
   {
       ObjectCode *o;
       int is_dup = 0;
       for (o = objects; o; o = o->next) {
          if (0 == pathcmp(o->fileName, path)) {
             is_dup = 1;
             break; /* don't need to search further */
          }
       }
       if (is_dup) {
          IF_DEBUG(linker, debugBelch(
            "GHCi runtime linker: warning: looks like you're trying to load the\n"
            "same object file twice:\n"
            "   %" PATH_FMT "\n"
            "GHCi will ignore this, but be warned.\n"
            , path));
          return 1; /* success */
       }
   }

   r = pathstat(path, &st);
   if (r == -1) {
       IF_DEBUG(linker, debugBelch("File doesn't exist\n"));
       return 0;
   }

   fileSize = st.st_size;

#ifdef USE_MMAP
   /* On many architectures malloc'd memory isn't executable, so we need to use mmap. */

#if defined(openbsd_HOST_OS)
   fd = open(path, O_RDONLY, S_IRUSR);
#else
   fd = open(path, O_RDONLY);
#endif
   if (fd == -1)
      barf("loadObj: can't open `%s'", path);

   image = mmapForLinker(fileSize, 0, fd);

   close(fd);

#else /* !USE_MMAP */
   /* load the image into memory */
   f = pathopen(path, WSTR("rb"));
   if (!f)
       barf("loadObj: can't read `%" PATH_FMT "'", path);

#   if defined(mingw32_HOST_OS)
        // TODO: We would like to use allocateExec here, but allocateExec
        //       cannot currently allocate blocks large enough.
    {
        int offset;
#if defined(x86_64_HOST_ARCH)
        /* We get back 8-byte aligned memory (is that guaranteed?), but
           the offsets to the sections within the file are all 4 mod 8
           (is that guaranteed?). We therefore need to offset the image
           by 4, so that all the pointers are 8-byte aligned, so that
           pointer tagging works. */
        offset = 4;
#else
        offset = 0;
#endif
      image = VirtualAlloc(NULL, fileSize + offset, MEM_RESERVE | MEM_COMMIT,
                           PAGE_EXECUTE_READWRITE);
      image += offset;
    }
#   elif defined(darwin_HOST_OS)
    // In a Mach-O .o file, all sections can and will be misaligned
    // if the total size of the headers is not a multiple of the
    // desired alignment. This is fine for .o files that only serve
    // as input for the static linker, but it's not fine for us,
    // as SSE (used by gcc for floating point) and Altivec require
    // 16-byte alignment.
    // We calculate the correct alignment from the header before
    // reading the file, and then we misalign image on purpose so
    // that the actual sections end up aligned again.
   misalignment = machoGetMisalignment(f);
   image = stgMallocBytes(fileSize + misalignment, "loadObj(image)");
   image += misalignment;
#  else
   image = stgMallocBytes(fileSize, "loadObj(image)");
#  endif

   {
       int n;
       n = fread ( image, 1, fileSize, f );
       if (n != fileSize)
           barf("loadObj: error whilst reading `%s'", path);
   }
   fclose(f);
#endif /* USE_MMAP */

   oc = mkOc(path, image, fileSize, NULL
#ifndef USE_MMAP
#ifdef darwin_HOST_OS
            , misalignment
#endif
#endif
            );

   return loadOc(oc);
}

static HsInt
loadOc( ObjectCode* oc ) {
   int r;

   IF_DEBUG(linker, debugBelch("loadOc: start\n"));

#  if defined(OBJFORMAT_MACHO) && (defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH))
   r = ocAllocateSymbolExtras_MachO ( oc );
   if (!r) {
       IF_DEBUG(linker, debugBelch("loadOc: ocAllocateSymbolExtras_MachO failed\n"));
       return r;
   }
#  elif defined(OBJFORMAT_ELF) && (defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH) || defined(arm_HOST_ARCH))
   r = ocAllocateSymbolExtras_ELF ( oc );
   if (!r) {
       IF_DEBUG(linker, debugBelch("loadOc: ocAllocateSymbolExtras_ELF failed\n"));
       return r;
   }
#endif

   /* verify the in-memory image */
#  if defined(OBJFORMAT_ELF)
   r = ocVerifyImage_ELF ( oc );
#  elif defined(OBJFORMAT_PEi386)
   r = ocVerifyImage_PEi386 ( oc );
#  elif defined(OBJFORMAT_MACHO)
   r = ocVerifyImage_MachO ( oc );
#  else
   barf("loadObj: no verify method");
#  endif
   if (!r) {
       IF_DEBUG(linker, debugBelch("loadOc: ocVerifyImage_* failed\n"));
       return r;
   }

   /* build the symbol list for this image */
#  if defined(OBJFORMAT_ELF)
   r = ocGetNames_ELF ( oc );
#  elif defined(OBJFORMAT_PEi386)
   r = ocGetNames_PEi386 ( oc );
#  elif defined(OBJFORMAT_MACHO)
   r = ocGetNames_MachO ( oc );
#  else
   barf("loadObj: no getNames method");
#  endif
   if (!r) {
       IF_DEBUG(linker, debugBelch("loadOc: ocGetNames_* failed\n"));
       return r;
   }

   /* loaded, but not resolved yet */
   oc->status = OBJECT_LOADED;
   IF_DEBUG(linker, debugBelch("loadOc: done.\n"));

   return 1;
}

/* -----------------------------------------------------------------------------
 * resolve all the currently unlinked objects in memory
 *
 * Returns: 1 if ok, 0 on error.
 */
HsInt
resolveObjs( void )
{
    ObjectCode *oc;
    int r;

    IF_DEBUG(linker, debugBelch("resolveObjs: start\n"));
    initLinker();

    for (oc = objects; oc; oc = oc->next) {
        if (oc->status != OBJECT_RESOLVED) {
#           if defined(OBJFORMAT_ELF)
            r = ocResolve_ELF ( oc );
#           elif defined(OBJFORMAT_PEi386)
            r = ocResolve_PEi386 ( oc );
#           elif defined(OBJFORMAT_MACHO)
            r = ocResolve_MachO ( oc );
#           else
            barf("resolveObjs: not implemented on this platform");
#           endif
            if (!r) { return r; }

            // run init/init_array/ctors/mod_init_func

            loading_obj = oc; // tells foreignExportStablePtr what to do
#if defined(OBJFORMAT_ELF)
            r = ocRunInit_ELF ( oc );
#elif defined(OBJFORMAT_PEi386)
            r = ocRunInit_PEi386 ( oc );
#elif defined(OBJFORMAT_MACHO)
            r = ocRunInit_MachO ( oc );
#else
            barf("resolveObjs: initializers not implemented on this platform");
#endif
            loading_obj = NULL;

            if (!r) { return r; }

            oc->status = OBJECT_RESOLVED;
        }
    }
    IF_DEBUG(linker, debugBelch("resolveObjs: done\n"));
    return 1;
}

/* -----------------------------------------------------------------------------
 * delete an object from the pool
 */
HsInt
unloadObj( pathchar *path )
{
    ObjectCode *oc, *prev, *next;
    HsBool unloadedAnyObj = HS_BOOL_FALSE;

    ASSERT(symhash != NULL);
    ASSERT(objects != NULL);

    initLinker();

    IF_DEBUG(linker, debugBelch("unloadObj: %" PATH_FMT "\n", path));

    prev = NULL;
    for (oc = objects; oc; prev = oc, oc = next) {
        next = oc->next;

        if (!pathcmp(oc->fileName,path)) {

            /* Remove all the mappings for the symbols within this
             * object..
             */
            {
                int i;
                for (i = 0; i < oc->n_symbols; i++) {
                   if (oc->symbols[i] != NULL) {
                       ghciRemoveSymbolTable(symhash, oc->symbols[i], oc);
                   }
                }
            }

            if (prev == NULL) {
                objects = oc->next;
            } else {
                prev->next = oc->next;
            }
            oc->next = unloaded_objects;
            unloaded_objects = oc;

            // The data itself and a few other bits (oc->fileName,
            // oc->archiveMemberName) are kept until freeObjectCode(),
            // which is only called when it has been determined that
            // it is safe to unload the object.
            stgFree(oc->symbols);

            {
                Section *s, *nexts;

                for (s = oc->sections; s != NULL; s = nexts) {
                    nexts = s->next;
                    stgFree(s);
                }
            }

            freeProddableBlocks(oc);

            // Release any StablePtrs that were created when this
            // object module was initialized.
            {
                ForeignExportStablePtr *fe_ptr, *next;

                for (fe_ptr = oc->stable_ptrs; fe_ptr != NULL; fe_ptr = next) {
                    next = fe_ptr->next;
                    freeStablePtr(fe_ptr->stable_ptr);
                    stgFree(fe_ptr);
                }
            }

            oc->status = OBJECT_UNLOADED;

            /* This could be a member of an archive so continue
             * unloading other members. */
            unloadedAnyObj = HS_BOOL_TRUE;
        }
    }

    if (unloadedAnyObj) {
        return 1;
    }
    else {
        errorBelch("unloadObj: can't find `%" PATH_FMT "' to unload", path);
        return 0;
    }
}

/* -----------------------------------------------------------------------------
 * Sanity checking.  For each ObjectCode, maintain a list of address ranges
 * which may be prodded during relocation, and abort if we try and write
 * outside any of these.
 */
static void
addProddableBlock ( ObjectCode* oc, void* start, int size )
{
   ProddableBlock* pb
      = stgMallocBytes(sizeof(ProddableBlock), "addProddableBlock");

   IF_DEBUG(linker, debugBelch("addProddableBlock: %p %p %d\n", oc, start, size));
   ASSERT(size > 0);
   pb->start      = start;
   pb->size       = size;
   pb->next       = oc->proddables;
   oc->proddables = pb;
}

static void
checkProddableBlock (ObjectCode *oc, void *addr, size_t size )
{
   ProddableBlock* pb;

   for (pb = oc->proddables; pb != NULL; pb = pb->next) {
      char* s = (char*)(pb->start);
      char* e = s + pb->size;
      char* a = (char*)addr;
      if (a >= s && (a+size) <= e) return;
   }
   barf("checkProddableBlock: invalid fixup in runtime linker: %p", addr);
}

static void freeProddableBlocks (ObjectCode *oc)
{
    ProddableBlock *pb, *next;

    for (pb = oc->proddables; pb != NULL; pb = next) {
        next = pb->next;
        stgFree(pb);
    }
    oc->proddables = NULL;
}

/* -----------------------------------------------------------------------------
 * Section management.
 */
static void
addSection ( ObjectCode* oc, SectionKind kind,
                         void* start, void* end )
{
   Section* s   = stgMallocBytes(sizeof(Section), "addSection");
   s->start     = start;
   s->end       = end;
   s->kind      = kind;
   s->next      = oc->sections;
   oc->sections = s;

   IF_DEBUG(linker, debugBelch("addSection: %p-%p (size %lld), kind %d\n",
                               start, ((char*)end)-1, ((long long)(size_t)end) - ((long long)(size_t)start) + 1, kind ));
}


/* --------------------------------------------------------------------------
 * Symbol Extras.
 * This is about allocating a small chunk of memory for every symbol in the
 * object file. We make sure that the SymboLExtras are always "in range" of
 * limited-range PC-relative instructions on various platforms by allocating
 * them right next to the object code itself.
 */

#if defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH) || defined(arm_HOST_ARCH)
#if !defined(x86_64_HOST_ARCH) || !defined(mingw32_HOST_OS)

/*
  ocAllocateSymbolExtras

  Allocate additional space at the end of the object file image to make room
  for jump islands (powerpc, x86_64, arm) and GOT entries (x86_64).

  PowerPC relative branch instructions have a 24 bit displacement field.
  As PPC code is always 4-byte-aligned, this yields a +-32MB range.
  If a particular imported symbol is outside this range, we have to redirect
  the jump to a short piece of new code that just loads the 32bit absolute
  address and jumps there.
  On x86_64, PC-relative jumps and PC-relative accesses to the GOT are limited
  to 32 bits (+-2GB).

  This function just allocates space for one SymbolExtra for every
  undefined symbol in the object file. The code for the jump islands is
  filled in by makeSymbolExtra below.
*/

static int ocAllocateSymbolExtras( ObjectCode* oc, int count, int first )
{
#ifdef USE_MMAP
  int pagesize, n, m;
#endif
  int aligned;
#ifndef USE_MMAP
  int misalignment = 0;
#ifdef darwin_HOST_OS
  misalignment = oc->misalignment;
#endif
#endif

  if( count > 0 )
  {
    // round up to the nearest 4
    aligned = (oc->fileSize + 3) & ~3;

#ifdef USE_MMAP
    pagesize = getpagesize();
    n = ROUND_UP( oc->fileSize, pagesize );
    m = ROUND_UP( aligned + sizeof (SymbolExtra) * count, pagesize );

    /* we try to use spare space at the end of the last page of the
     * image for the jump islands, but if there isn't enough space
     * then we have to map some (anonymously, remembering MAP_32BIT).
     */
    if( m > n ) // we need to allocate more pages
    {
        if (USE_CONTIGUOUS_MMAP)
        {
            /* Keep image and symbol_extras contiguous */
            void *new = mmapForLinker(n + (sizeof(SymbolExtra) * count),
                                  MAP_ANONYMOUS, -1);
            if (new)
            {
                memcpy(new, oc->image, oc->fileSize);
                munmap(oc->image, n);
                oc->image = new;
                oc->fileSize = n + (sizeof(SymbolExtra) * count);
                oc->symbol_extras = (SymbolExtra *) (oc->image + n);
            }
            else
                oc->symbol_extras = NULL;
        }
        else
        {
            oc->symbol_extras = mmapForLinker(sizeof(SymbolExtra) * count,
                                          MAP_ANONYMOUS, -1);
        }
    }
    else
    {
        oc->symbol_extras = (SymbolExtra *) (oc->image + aligned);
    }
#else
    oc->image -= misalignment;
    oc->image = stgReallocBytes( oc->image,
                                 misalignment +
                                 aligned + sizeof (SymbolExtra) * count,
                                 "ocAllocateSymbolExtras" );
    oc->image += misalignment;

    oc->symbol_extras = (SymbolExtra *) (oc->image + aligned);
#endif /* USE_MMAP */

    memset( oc->symbol_extras, 0, sizeof (SymbolExtra) * count );
  }
  else
    oc->symbol_extras = NULL;

  oc->first_symbol_extra = first;
  oc->n_symbol_extras = count;

  return 1;
}

#endif
#endif // defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH) || defined(arm_HOST_ARCH)

#if defined(arm_HOST_ARCH)

static void
ocFlushInstructionCache( ObjectCode *oc )
{
    // Object code
    __clear_cache(oc->image, oc->image + oc->fileSize);
    // Jump islands
    __clear_cache(oc->symbol_extras, &oc->symbol_extras[oc->n_symbol_extras]);
}

#endif

#if defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
#if !defined(x86_64_HOST_ARCH) || !defined(mingw32_HOST_OS)

static SymbolExtra* makeSymbolExtra( ObjectCode* oc,
                                     unsigned long symbolNumber,
                                     unsigned long target )
{
  SymbolExtra *extra;

  ASSERT( symbolNumber >= oc->first_symbol_extra
        && symbolNumber - oc->first_symbol_extra < oc->n_symbol_extras);

  extra = &oc->symbol_extras[symbolNumber - oc->first_symbol_extra];

#ifdef powerpc_HOST_ARCH
  // lis r12, hi16(target)
  extra->jumpIsland.lis_r12     = 0x3d80;
  extra->jumpIsland.hi_addr     = target >> 16;

  // ori r12, r12, lo16(target)
  extra->jumpIsland.ori_r12_r12 = 0x618c;
  extra->jumpIsland.lo_addr     = target & 0xffff;

  // mtctr r12
  extra->jumpIsland.mtctr_r12   = 0x7d8903a6;

  // bctr
  extra->jumpIsland.bctr        = 0x4e800420;
#endif
#ifdef x86_64_HOST_ARCH
  // jmp *-14(%rip)
  static uint8_t jmp[] = { 0xFF, 0x25, 0xF2, 0xFF, 0xFF, 0xFF };
  extra->addr = target;
  memcpy(extra->jumpIsland, jmp, 6);
#endif

  return extra;
}

#endif
#endif // defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)

#ifdef arm_HOST_ARCH
static SymbolExtra* makeArmSymbolExtra( ObjectCode* oc,
                                        unsigned long symbolNumber,
                                        unsigned long target,
                                        int fromThumb,
                                        int toThumb )
{
  SymbolExtra *extra;

  ASSERT( symbolNumber >= oc->first_symbol_extra
        && symbolNumber - oc->first_symbol_extra < oc->n_symbol_extras);

  extra = &oc->symbol_extras[symbolNumber - oc->first_symbol_extra];

  // Make sure instruction mode bit is set properly
  if (toThumb)
    target |= 1;
  else
    target &= ~1;

  if (!fromThumb) {
    // In ARM encoding:
    //   movw r12, #0
    //   movt r12, #0
    //   bx r12
    uint32_t code[] = { 0xe300c000, 0xe340c000, 0xe12fff1c };

    // Patch lower half-word into movw
    code[0] |= ((target>>12) & 0xf) << 16;
    code[0] |= target & 0xfff;
    // Patch upper half-word into movt
    target >>= 16;
    code[1] |= ((target>>12) & 0xf) << 16;
    code[1] |= target & 0xfff;

    memcpy(extra->jumpIsland, code, 12);

  } else {
    // In Thumb encoding:
    //   movw r12, #0
    //   movt r12, #0
    //   bx r12
    uint16_t code[] = { 0xf240,  0x0c00,
                        0xf2c0,  0x0c00,
                        0x4760 };

    // Patch lower half-word into movw
    code[0] |= (target>>12) & 0xf;
    code[0] |= ((target>>11) & 0x1) << 10;
    code[1] |= ((target>>8) & 0x7) << 12;
    code[1] |= target & 0xff;
    // Patch upper half-word into movt
    target >>= 16;
    code[2] |= (target>>12) & 0xf;
    code[2] |= ((target>>11) & 0x1) << 10;
    code[3] |= ((target>>8) & 0x7) << 12;
    code[3] |= target & 0xff;

    memcpy(extra->jumpIsland, code, 10);
  }

  return extra;
}
#endif // arm_HOST_ARCH

/* --------------------------------------------------------------------------
 * PowerPC specifics (instruction cache flushing)
 * ------------------------------------------------------------------------*/

#ifdef powerpc_HOST_ARCH
/*
   ocFlushInstructionCache

   Flush the data & instruction caches.
   Because the PPC has split data/instruction caches, we have to
   do that whenever we modify code at runtime.
 */

static void
ocFlushInstructionCacheFrom(void* begin, size_t length)
{
    size_t         n = (length + 3) / 4;
    unsigned long* p = begin;

    while (n--)
    {
        __asm__ volatile ( "dcbf 0,%0\n\t"
                           "sync\n\t"
                           "icbi 0,%0"
                           :
                           : "r" (p)
                         );
        p++;
    }
    __asm__ volatile ( "sync\n\t"
                       "isync"
                     );
}

static void
ocFlushInstructionCache( ObjectCode *oc )
{
    /* The main object code */
    ocFlushInstructionCacheFrom(oc->image
#ifdef darwin_HOST_OS
            + oc->misalignment
#endif
            , oc->fileSize);

    /* Jump Islands */
    ocFlushInstructionCacheFrom(oc->symbol_extras, sizeof(SymbolExtra) * oc->n_symbol_extras);
}
#endif /* powerpc_HOST_ARCH */


/* --------------------------------------------------------------------------
 * PEi386 specifics (Win32 targets)
 * ------------------------------------------------------------------------*/

/* The information for this linker comes from
      Microsoft Portable Executable
      and Common Object File Format Specification
      revision 5.1 January 1998
   which SimonM says comes from the MS Developer Network CDs.

   It can be found there (on older CDs), but can also be found
   online at:

      http://www.microsoft.com/hwdev/hardware/PECOFF.asp

   (this is Rev 6.0 from February 1999).

   Things move, so if that fails, try searching for it via

      http://www.google.com/search?q=PE+COFF+specification

   The ultimate reference for the PE format is the Winnt.h
   header file that comes with the Platform SDKs; as always,
   implementations will drift wrt their documentation.

   A good background article on the PE format is Matt Pietrek's
   March 1994 article in Microsoft System Journal (MSJ)
   (Vol.9, No. 3): "Peering Inside the PE: A Tour of the
   Win32 Portable Executable File Format." The info in there
   has recently been updated in a two part article in
   MSDN magazine, issues Feb and March 2002,
   "Inside Windows: An In-Depth Look into the Win32 Portable
   Executable File Format"

   John Levine's book "Linkers and Loaders" contains useful
   info on PE too.
*/


#if defined(OBJFORMAT_PEi386)



typedef unsigned char          UChar;
typedef unsigned short         UInt16;
typedef unsigned int           UInt32;
typedef          int           Int32;
typedef unsigned long long int UInt64;


typedef
   struct {
      UInt16 Machine;
      UInt16 NumberOfSections;
      UInt32 TimeDateStamp;
      UInt32 PointerToSymbolTable;
      UInt32 NumberOfSymbols;
      UInt16 SizeOfOptionalHeader;
      UInt16 Characteristics;
   }
   COFF_header;

#define sizeof_COFF_header 20


typedef
   struct {
      UChar  Name[8];
      UInt32 VirtualSize;
      UInt32 VirtualAddress;
      UInt32 SizeOfRawData;
      UInt32 PointerToRawData;
      UInt32 PointerToRelocations;
      UInt32 PointerToLinenumbers;
      UInt16 NumberOfRelocations;
      UInt16 NumberOfLineNumbers;
      UInt32 Characteristics;
   }
   COFF_section;

#define sizeof_COFF_section 40


typedef
   struct {
      UChar  Name[8];
      UInt32 Value;
      UInt16 SectionNumber;
      UInt16 Type;
      UChar  StorageClass;
      UChar  NumberOfAuxSymbols;
   }
   COFF_symbol;

#define sizeof_COFF_symbol 18


typedef
   struct {
      UInt32 VirtualAddress;
      UInt32 SymbolTableIndex;
      UInt16 Type;
   }
   COFF_reloc;

#define sizeof_COFF_reloc 10


/* From PE spec doc, section 3.3.2 */
/* Note use of MYIMAGE_* since IMAGE_* are already defined in
   windows.h -- for the same purpose, but I want to know what I'm
   getting, here. */
#define MYIMAGE_FILE_RELOCS_STRIPPED     0x0001
#define MYIMAGE_FILE_EXECUTABLE_IMAGE    0x0002
#define MYIMAGE_FILE_DLL                 0x2000
#define MYIMAGE_FILE_SYSTEM              0x1000
#define MYIMAGE_FILE_BYTES_REVERSED_HI   0x8000
#define MYIMAGE_FILE_BYTES_REVERSED_LO   0x0080
#define MYIMAGE_FILE_32BIT_MACHINE       0x0100

/* From PE spec doc, section 5.4.2 and 5.4.4 */
#define MYIMAGE_SYM_CLASS_EXTERNAL       2
#define MYIMAGE_SYM_CLASS_STATIC         3
#define MYIMAGE_SYM_UNDEFINED            0

/* From PE spec doc, section 4.1 */
#define MYIMAGE_SCN_CNT_CODE             0x00000020
#define MYIMAGE_SCN_CNT_INITIALIZED_DATA 0x00000040
#define MYIMAGE_SCN_LNK_NRELOC_OVFL      0x01000000

/* From PE spec doc, section 5.2.1 */
#define MYIMAGE_REL_I386_DIR32           0x0006
#define MYIMAGE_REL_I386_REL32           0x0014


/* We use myindex to calculate array addresses, rather than
   simply doing the normal subscript thing.  That's because
   some of the above structs have sizes which are not
   a whole number of words.  GCC rounds their sizes up to a
   whole number of words, which means that the address calcs
   arising from using normal C indexing or pointer arithmetic
   are just plain wrong.  Sigh.
*/
static UChar *
myindex ( int scale, void* base, int index )
{
   return
      ((UChar*)base) + scale * index;
}


static void
printName ( UChar* name, UChar* strtab )
{
   if (name[0]==0 && name[1]==0 && name[2]==0 && name[3]==0) {
      UInt32 strtab_offset = * (UInt32*)(name+4);
      debugBelch("%s", strtab + strtab_offset );
   } else {
      int i;
      for (i = 0; i < 8; i++) {
         if (name[i] == 0) break;
         debugBelch("%c", name[i] );
      }
   }
}


static void
copyName ( UChar* name, UChar* strtab, UChar* dst, int dstSize )
{
   if (name[0]==0 && name[1]==0 && name[2]==0 && name[3]==0) {
      UInt32 strtab_offset = * (UInt32*)(name+4);
      strncpy ( (char*)dst, (char*)strtab+strtab_offset, dstSize );
      dst[dstSize-1] = 0;
   } else {
      int i = 0;
      while (1) {
         if (i >= 8) break;
         if (name[i] == 0) break;
         dst[i] = name[i];
         i++;
      }
      dst[i] = 0;
   }
}


static UChar *
cstring_from_COFF_symbol_name ( UChar* name, UChar* strtab )
{
   UChar* newstr;
   /* If the string is longer than 8 bytes, look in the
      string table for it -- this will be correctly zero terminated.
   */
   if (name[0]==0 && name[1]==0 && name[2]==0 && name[3]==0) {
      UInt32 strtab_offset = * (UInt32*)(name+4);
      return ((UChar*)strtab) + strtab_offset;
   }
   /* Otherwise, if shorter than 8 bytes, return the original,
      which by defn is correctly terminated.
   */
   if (name[7]==0) return name;
   /* The annoying case: 8 bytes.  Copy into a temporary
      (XXX which is never freed ...)
   */
   newstr = stgMallocBytes(9, "cstring_from_COFF_symbol_name");
   ASSERT(newstr);
   strncpy((char*)newstr,(char*)name,8);
   newstr[8] = 0;
   return newstr;
}

/* Getting the name of a section is mildly tricky, so we make a
   function for it.  Sadly, in one case we have to copy the string
   (when it is exactly 8 bytes long there's no trailing '\0'), so for
   consistency we *always* copy the string; the caller must free it
*/
static char *
cstring_from_section_name (UChar* name, UChar* strtab)
{
    char *newstr;

    if (name[0]=='/') {
        int strtab_offset = strtol((char*)name+1,NULL,10);
        int len = strlen(((char*)strtab) + strtab_offset);

        newstr = stgMallocBytes(len+1, "cstring_from_section_symbol_name");
        strcpy((char*)newstr, (char*)((UChar*)strtab) + strtab_offset);
        return newstr;
    }
    else
    {
        newstr = stgMallocBytes(9, "cstring_from_section_symbol_name");
        ASSERT(newstr);
        strncpy((char*)newstr,(char*)name,8);
        newstr[8] = 0;
        return newstr;
    }
}

/* Just compares the short names (first 8 chars) */
static COFF_section *
findPEi386SectionCalled ( ObjectCode* oc,  UChar* name )
{
   int i;
   COFF_header* hdr
      = (COFF_header*)(oc->image);
   COFF_section* sectab
      = (COFF_section*) (
           ((UChar*)(oc->image))
           + sizeof_COFF_header + hdr->SizeOfOptionalHeader
        );
   for (i = 0; i < hdr->NumberOfSections; i++) {
      UChar* n1;
      UChar* n2;
      COFF_section* section_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, sectab, i );
      n1 = (UChar*) &(section_i->Name);
      n2 = name;
      if (n1[0]==n2[0] && n1[1]==n2[1] && n1[2]==n2[2] &&
          n1[3]==n2[3] && n1[4]==n2[4] && n1[5]==n2[5] &&
          n1[6]==n2[6] && n1[7]==n2[7])
         return section_i;
   }

   return NULL;
}

static void
zapTrailingAtSign ( UChar* sym )
{
#  define my_isdigit(c) ((c) >= '0' && (c) <= '9')
   int i, j;
   if (sym[0] == 0) return;
   i = 0;
   while (sym[i] != 0) i++;
   i--;
   j = i;
   while (j > 0 && my_isdigit(sym[j])) j--;
   if (j > 0 && sym[j] == '@' && j != i) sym[j] = 0;
#  undef my_isdigit
}

static void *
lookupSymbolInDLLs ( UChar *lbl )
{
    OpenedDLL* o_dll;
    void *sym;

    for (o_dll = opened_dlls; o_dll != NULL; o_dll = o_dll->next) {
        /* debugBelch("look in %s for %s\n", o_dll->name, lbl); */

        if (lbl[0] == '_') {
            /* HACK: if the name has an initial underscore, try stripping
               it off & look that up first. I've yet to verify whether there's
               a Rule that governs whether an initial '_' *should always* be
               stripped off when mapping from import lib name to the DLL name.
            */
            sym = GetProcAddress(o_dll->instance, (char*)(lbl+1));
            if (sym != NULL) {
                /*debugBelch("found %s in %s\n", lbl+1,o_dll->name);*/
                return sym;
            }
        }
        sym = GetProcAddress(o_dll->instance, (char*)lbl);
        if (sym != NULL) {
            /*debugBelch("found %s in %s\n", lbl,o_dll->name);*/
            return sym;
           }
    }
    return NULL;
}


static int
ocVerifyImage_PEi386 ( ObjectCode* oc )
{
   int i;
   UInt32 j, noRelocs;
   COFF_header*  hdr;
   COFF_section* sectab;
   COFF_symbol*  symtab;
   UChar*        strtab;
   /* debugBelch("\nLOADING %s\n", oc->fileName); */
   hdr = (COFF_header*)(oc->image);
   sectab = (COFF_section*) (
               ((UChar*)(oc->image))
               + sizeof_COFF_header + hdr->SizeOfOptionalHeader
            );
   symtab = (COFF_symbol*) (
               ((UChar*)(oc->image))
               + hdr->PointerToSymbolTable
            );
   strtab = ((UChar*)symtab)
            + hdr->NumberOfSymbols * sizeof_COFF_symbol;

#if defined(i386_HOST_ARCH)
   if (hdr->Machine != 0x14c) {
      errorBelch("%" PATH_FMT ": Not x86 PEi386", oc->fileName);
      return 0;
   }
#elif defined(x86_64_HOST_ARCH)
   if (hdr->Machine != 0x8664) {
      errorBelch("%" PATH_FMT ": Not x86_64 PEi386", oc->fileName);
      return 0;
   }
#else
   errorBelch("PEi386 not supported on this arch");
#endif

   if (hdr->SizeOfOptionalHeader != 0) {
      errorBelch("%" PATH_FMT ": PEi386 with nonempty optional header", oc->fileName);
      return 0;
   }
   if ( /* (hdr->Characteristics & MYIMAGE_FILE_RELOCS_STRIPPED) || */
        (hdr->Characteristics & MYIMAGE_FILE_EXECUTABLE_IMAGE) ||
        (hdr->Characteristics & MYIMAGE_FILE_DLL) ||
        (hdr->Characteristics & MYIMAGE_FILE_SYSTEM) ) {
      errorBelch("%" PATH_FMT ": Not a PEi386 object file", oc->fileName);
      return 0;
   }
   if ( (hdr->Characteristics & MYIMAGE_FILE_BYTES_REVERSED_HI)
        /* || !(hdr->Characteristics & MYIMAGE_FILE_32BIT_MACHINE) */ ) {
      errorBelch("%" PATH_FMT ": Invalid PEi386 word size or endiannness: %d",
                 oc->fileName,
                 (int)(hdr->Characteristics));
      return 0;
   }
   /* If the string table size is way crazy, this might indicate that
      there are more than 64k relocations, despite claims to the
      contrary.  Hence this test. */
   /* debugBelch("strtab size %d\n", * (UInt32*)strtab); */
#if 0
   if ( (*(UInt32*)strtab) > 600000 ) {
      /* Note that 600k has no special significance other than being
         big enough to handle the almost-2MB-sized lumps that
         constitute HSwin32*.o. */
      debugBelch("PEi386 object has suspiciously large string table; > 64k relocs?");
      return 0;
   }
#endif

   /* No further verification after this point; only debug printing. */
   i = 0;
   IF_DEBUG(linker, i=1);
   if (i == 0) return 1;

   debugBelch( "sectab offset = %" FMT_Int "\n", ((UChar*)sectab) - ((UChar*)hdr) );
   debugBelch( "symtab offset = %" FMT_Int "\n", ((UChar*)symtab) - ((UChar*)hdr) );
   debugBelch( "strtab offset = %" FMT_Int "\n", ((UChar*)strtab) - ((UChar*)hdr) );

   debugBelch("\n" );
   debugBelch( "Machine:           0x%x\n", (UInt32)(hdr->Machine) );
   debugBelch( "# sections:        %d\n",   (UInt32)(hdr->NumberOfSections) );
   debugBelch( "time/date:         0x%x\n", (UInt32)(hdr->TimeDateStamp) );
   debugBelch( "symtab offset:     %d\n",   (UInt32)(hdr->PointerToSymbolTable) );
   debugBelch( "# symbols:         %d\n",   (UInt32)(hdr->NumberOfSymbols) );
   debugBelch( "sz of opt hdr:     %d\n",   (UInt32)(hdr->SizeOfOptionalHeader) );
   debugBelch( "characteristics:   0x%x\n", (UInt32)(hdr->Characteristics) );

   /* Print the section table. */
   debugBelch("\n" );
   for (i = 0; i < hdr->NumberOfSections; i++) {
      COFF_reloc* reltab;
      COFF_section* sectab_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, sectab, i );
      debugBelch(
                "\n"
                "section %d\n"
                "     name `",
                i
              );
      printName ( sectab_i->Name, strtab );
      debugBelch(
                "'\n"
                "    vsize %d\n"
                "    vaddr %d\n"
                "  data sz %d\n"
                " data off %d\n"
                "  num rel %d\n"
                "  off rel %d\n"
                "  ptr raw 0x%x\n",
                sectab_i->VirtualSize,
                sectab_i->VirtualAddress,
                sectab_i->SizeOfRawData,
                sectab_i->PointerToRawData,
                sectab_i->NumberOfRelocations,
                sectab_i->PointerToRelocations,
                sectab_i->PointerToRawData
              );
      reltab = (COFF_reloc*) (
                  ((UChar*)(oc->image)) + sectab_i->PointerToRelocations
               );

      if ( sectab_i->Characteristics & MYIMAGE_SCN_LNK_NRELOC_OVFL ) {
        /* If the relocation field (a short) has overflowed, the
         * real count can be found in the first reloc entry.
         *
         * See Section 4.1 (last para) of the PE spec (rev6.0).
         */
        COFF_reloc* rel = (COFF_reloc*)
                           myindex ( sizeof_COFF_reloc, reltab, 0 );
        noRelocs = rel->VirtualAddress;
        j = 1;
      } else {
        noRelocs = sectab_i->NumberOfRelocations;
        j = 0;
      }

      for (; j < noRelocs; j++) {
         COFF_symbol* sym;
         COFF_reloc* rel = (COFF_reloc*)
                           myindex ( sizeof_COFF_reloc, reltab, j );
         debugBelch(
                   "        type 0x%-4x   vaddr 0x%-8x   name `",
                   (UInt32)rel->Type,
                   rel->VirtualAddress );
         sym = (COFF_symbol*)
               myindex ( sizeof_COFF_symbol, symtab, rel->SymbolTableIndex );
         /* Hmm..mysterious looking offset - what's it for? SOF */
         printName ( sym->Name, strtab -10 );
         debugBelch("'\n" );
      }

      debugBelch("\n" );
   }
   debugBelch("\n" );
   debugBelch("string table has size 0x%x\n", * (UInt32*)strtab );
   debugBelch("---START of string table---\n");
   for (i = 4; i < *(Int32*)strtab; i++) {
      if (strtab[i] == 0)
         debugBelch("\n"); else
         debugBelch("%c", strtab[i] );
   }
   debugBelch("--- END  of string table---\n");

   debugBelch("\n" );
   i = 0;
   while (1) {
      COFF_symbol* symtab_i;
      if (i >= (Int32)(hdr->NumberOfSymbols)) break;
      symtab_i = (COFF_symbol*)
                 myindex ( sizeof_COFF_symbol, symtab, i );
      debugBelch(
                "symbol %d\n"
                "     name `",
                i
              );
      printName ( symtab_i->Name, strtab );
      debugBelch(
                "'\n"
                "    value 0x%x\n"
                "   1+sec# %d\n"
                "     type 0x%x\n"
                "   sclass 0x%x\n"
                "     nAux %d\n",
                symtab_i->Value,
                (Int32)(symtab_i->SectionNumber),
                (UInt32)symtab_i->Type,
                (UInt32)symtab_i->StorageClass,
                (UInt32)symtab_i->NumberOfAuxSymbols
              );
      i += symtab_i->NumberOfAuxSymbols;
      i++;
   }

   debugBelch("\n" );
   return 1;
}


static int
ocGetNames_PEi386 ( ObjectCode* oc )
{
   COFF_header*  hdr;
   COFF_section* sectab;
   COFF_symbol*  symtab;
   UChar*        strtab;

   UChar* sname;
   void*  addr;
   int    i;

   hdr = (COFF_header*)(oc->image);
   sectab = (COFF_section*) (
               ((UChar*)(oc->image))
               + sizeof_COFF_header + hdr->SizeOfOptionalHeader
            );
   symtab = (COFF_symbol*) (
               ((UChar*)(oc->image))
               + hdr->PointerToSymbolTable
            );
   strtab = ((UChar*)(oc->image))
            + hdr->PointerToSymbolTable
            + hdr->NumberOfSymbols * sizeof_COFF_symbol;

   /* Allocate space for any (local, anonymous) .bss sections. */

   for (i = 0; i < hdr->NumberOfSections; i++) {
      UInt32 bss_sz;
      UChar* zspace;
      COFF_section* sectab_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, sectab, i );

      char *secname = cstring_from_section_name(sectab_i->Name, strtab);

      if (0 != strcmp(secname, ".bss")) {
          stgFree(secname);
          continue;
      }

      stgFree(secname);

      /* sof 10/05: the PE spec text isn't too clear regarding what
       * the SizeOfRawData field is supposed to hold for object
       * file sections containing just uninitialized data -- for executables,
       * it is supposed to be zero; unclear what it's supposed to be
       * for object files. However, VirtualSize is guaranteed to be
       * zero for object files, which definitely suggests that SizeOfRawData
       * will be non-zero (where else would the size of this .bss section be
       * stored?) Looking at the COFF_section info for incoming object files,
       * this certainly appears to be the case.
       *
       * => I suspect we've been incorrectly handling .bss sections in (relocatable)
       * object files up until now. This turned out to bite us with ghc-6.4.1's use
       * of gcc-3.4.x, which has started to emit initially-zeroed-out local 'static'
       * variable decls into the .bss section. (The specific function in Q which
       * triggered this is libraries/base/cbits/dirUtils.c:__hscore_getFolderPath())
       */
      if (sectab_i->VirtualSize == 0 && sectab_i->SizeOfRawData == 0) continue;
      /* This is a non-empty .bss section.  Allocate zeroed space for
         it, and set its PointerToRawData field such that oc->image +
         PointerToRawData == addr_of_zeroed_space.  */
      bss_sz = sectab_i->VirtualSize;
      if ( bss_sz < sectab_i->SizeOfRawData) { bss_sz = sectab_i->SizeOfRawData; }
      zspace = stgCallocBytes(1, bss_sz, "ocGetNames_PEi386(anonymous bss)");
      sectab_i->PointerToRawData = ((UChar*)zspace) - ((UChar*)(oc->image));
      addProddableBlock(oc, zspace, bss_sz);
      /* debugBelch("BSS anon section at 0x%x\n", zspace); */
   }

   /* Copy section information into the ObjectCode. */

   for (i = 0; i < hdr->NumberOfSections; i++) {
      UChar* start;
      UChar* end;
      UInt32 sz;

      SectionKind kind
         = SECTIONKIND_OTHER;
      COFF_section* sectab_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, sectab, i );

      char *secname = cstring_from_section_name(sectab_i->Name, strtab);

      IF_DEBUG(linker, debugBelch("section name = %s\n", secname ));

#     if 0
      /* I'm sure this is the Right Way to do it.  However, the
         alternative of testing the sectab_i->Name field seems to
         work ok with Cygwin.

         EZY: We should strongly consider using this style, because
         it lets us pick up sections that should be added (e.g.
         for a while the linker did not work due to missing .eh_frame
         in this section.)
      */
      if (sectab_i->Characteristics & MYIMAGE_SCN_CNT_CODE ||
          sectab_i->Characteristics & MYIMAGE_SCN_CNT_INITIALIZED_DATA)
         kind = SECTIONKIND_CODE_OR_RODATA;
#     endif

      if (0==strcmp(".text",(char*)secname) ||
          0==strcmp(".text.startup",(char*)secname) ||
          0==strcmp(".rdata",(char*)secname)||
          0==strcmp(".eh_frame", (char*)secname)||
          0==strcmp(".rodata",(char*)secname))
         kind = SECTIONKIND_CODE_OR_RODATA;
      if (0==strcmp(".data",(char*)secname) ||
          0==strcmp(".bss",(char*)secname))
         kind = SECTIONKIND_RWDATA;
      if (0==strcmp(".ctors", (char*)secname))
         kind = SECTIONKIND_INIT_ARRAY;

      ASSERT(sectab_i->SizeOfRawData == 0 || sectab_i->VirtualSize == 0);
      sz = sectab_i->SizeOfRawData;
      if (sz < sectab_i->VirtualSize) sz = sectab_i->VirtualSize;

      start = ((UChar*)(oc->image)) + sectab_i->PointerToRawData;
      end   = start + sz - 1;

      if (kind == SECTIONKIND_OTHER
          /* Ignore sections called which contain stabs debugging
             information. */
          && 0 != strcmp(".stab", (char*)secname)
          && 0 != strcmp(".stabstr", (char*)secname)
          /* Ignore sections called which contain exception information. */
          && 0 != strcmp(".pdata", (char*)secname)
          && 0 != strcmp(".xdata", (char*)secname)
          /* ignore section generated from .ident */
          && 0!= strncmp(".debug", (char*)secname, 6)
          /* ignore unknown section that appeared in gcc 3.4.5(?) */
          && 0!= strcmp(".reloc", (char*)secname)
          && 0 != strcmp(".rdata$zzz", (char*)secname)
          /* ignore linker directive sections */
          && 0 != strcmp(".drectve", (char*)secname)
         ) {
         errorBelch("Unknown PEi386 section name `%s' (while processing: %" PATH_FMT")", secname, oc->fileName);
         stgFree(secname);
         return 0;
      }

      if (kind != SECTIONKIND_OTHER && end >= start) {
          if ((((size_t)(start)) % sizeof(void *)) != 0) {
              barf("Misaligned section: %p", start);
          }

         addSection(oc, kind, start, end);
         addProddableBlock(oc, start, end - start + 1);
      }

      stgFree(secname);
   }

   /* Copy exported symbols into the ObjectCode. */

   oc->n_symbols = hdr->NumberOfSymbols;
   oc->symbols   = stgMallocBytes(oc->n_symbols * sizeof(char*),
                                  "ocGetNames_PEi386(oc->symbols)");
   /* Call me paranoid; I don't care. */
   for (i = 0; i < oc->n_symbols; i++)
      oc->symbols[i] = NULL;

   i = 0;
   while (1) {
      COFF_symbol* symtab_i;
      if (i >= (Int32)(hdr->NumberOfSymbols)) break;
      symtab_i = (COFF_symbol*)
                 myindex ( sizeof_COFF_symbol, symtab, i );

      addr  = NULL;

      if (symtab_i->StorageClass == MYIMAGE_SYM_CLASS_EXTERNAL
          && symtab_i->SectionNumber != MYIMAGE_SYM_UNDEFINED) {
         /* This symbol is global and defined, viz, exported */
         /* for MYIMAGE_SYMCLASS_EXTERNAL
                && !MYIMAGE_SYM_UNDEFINED,
            the address of the symbol is:
                address of relevant section + offset in section
         */
         COFF_section* sectabent
            = (COFF_section*) myindex ( sizeof_COFF_section,
                                        sectab,
                                        symtab_i->SectionNumber-1 );
         addr = ((UChar*)(oc->image))
                + (sectabent->PointerToRawData
                   + symtab_i->Value);
      }
      else
      if (symtab_i->SectionNumber == MYIMAGE_SYM_UNDEFINED
          && symtab_i->Value > 0) {
         /* This symbol isn't in any section at all, ie, global bss.
            Allocate zeroed space for it. */
         addr = stgCallocBytes(1, symtab_i->Value,
                               "ocGetNames_PEi386(non-anonymous bss)");
         addSection(oc, SECTIONKIND_RWDATA, addr,
                        ((UChar*)addr) + symtab_i->Value - 1);
         addProddableBlock(oc, addr, symtab_i->Value);
         /* debugBelch("BSS      section at 0x%x\n", addr); */
      }

      if (addr != NULL ) {
         sname = cstring_from_COFF_symbol_name ( symtab_i->Name, strtab );
         /* debugBelch("addSymbol %p `%s \n", addr,sname);  */
         IF_DEBUG(linker, debugBelch("addSymbol %p `%s'\n", addr,sname);)
         ASSERT(i >= 0 && i < oc->n_symbols);
         /* cstring_from_COFF_symbol_name always succeeds. */
         oc->symbols[i] = (char*)sname;
         ghciInsertSymbolTable(oc->fileName, symhash, (char*)sname, addr,
            HS_BOOL_FALSE, oc);
      } else {
#        if 0
         debugBelch(
                   "IGNORING symbol %d\n"
                   "     name `",
                   i
                 );
         printName ( symtab_i->Name, strtab );
         debugBelch(
                   "'\n"
                   "    value 0x%x\n"
                   "   1+sec# %d\n"
                   "     type 0x%x\n"
                   "   sclass 0x%x\n"
                   "     nAux %d\n",
                   symtab_i->Value,
                   (Int32)(symtab_i->SectionNumber),
                   (UInt32)symtab_i->Type,
                   (UInt32)symtab_i->StorageClass,
                   (UInt32)symtab_i->NumberOfAuxSymbols
                 );
#        endif
      }

      i += symtab_i->NumberOfAuxSymbols;
      i++;
   }

   return 1;
}


static int
ocResolve_PEi386 ( ObjectCode* oc )
{
   COFF_header*  hdr;
   COFF_section* sectab;
   COFF_symbol*  symtab;
   UChar*        strtab;

   UInt32        A;
   size_t        S;
   void *        pP;

   int i;
   UInt32 j, noRelocs;

   /* ToDo: should be variable-sized?  But is at least safe in the
      sense of buffer-overrun-proof. */
   UChar symbol[1000];
   /* debugBelch("resolving for %s\n", oc->fileName); */

   hdr = (COFF_header*)(oc->image);
   sectab = (COFF_section*) (
               ((UChar*)(oc->image))
               + sizeof_COFF_header + hdr->SizeOfOptionalHeader
            );
   symtab = (COFF_symbol*) (
               ((UChar*)(oc->image))
               + hdr->PointerToSymbolTable
            );
   strtab = ((UChar*)(oc->image))
            + hdr->PointerToSymbolTable
            + hdr->NumberOfSymbols * sizeof_COFF_symbol;

   for (i = 0; i < hdr->NumberOfSections; i++) {
      COFF_section* sectab_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, sectab, i );
      COFF_reloc* reltab
         = (COFF_reloc*) (
              ((UChar*)(oc->image)) + sectab_i->PointerToRelocations
           );

      char *secname = cstring_from_section_name(sectab_i->Name, strtab);

      /* Ignore sections called which contain stabs debugging
         information. */
      if (0 == strcmp(".stab", (char*)secname)
          || 0 == strcmp(".stabstr", (char*)secname)
          || 0 == strcmp(".pdata", (char*)secname)
          || 0 == strcmp(".xdata", (char*)secname)
          || 0 == strncmp(".debug", (char*)secname, 6)
          || 0 == strcmp(".rdata$zzz", (char*)secname)) {
          stgFree(secname);
          continue;
      }

      stgFree(secname);

      if ( sectab_i->Characteristics & MYIMAGE_SCN_LNK_NRELOC_OVFL ) {
        /* If the relocation field (a short) has overflowed, the
         * real count can be found in the first reloc entry.
         *
         * See Section 4.1 (last para) of the PE spec (rev6.0).
         *
         * Nov2003 update: the GNU linker still doesn't correctly
         * handle the generation of relocatable object files with
         * overflown relocations. Hence the output to warn of potential
         * troubles.
         */
        COFF_reloc* rel = (COFF_reloc*)
                           myindex ( sizeof_COFF_reloc, reltab, 0 );
        noRelocs = rel->VirtualAddress;

        /* 10/05: we now assume (and check for) a GNU ld that is capable
         * of handling object files with (>2^16) of relocs.
         */
#if 0
        debugBelch("WARNING: Overflown relocation field (# relocs found: %u)\n",
                   noRelocs);
#endif
        j = 1;
      } else {
        noRelocs = sectab_i->NumberOfRelocations;
        j = 0;
      }

      for (; j < noRelocs; j++) {
         COFF_symbol* sym;
         COFF_reloc* reltab_j
            = (COFF_reloc*)
              myindex ( sizeof_COFF_reloc, reltab, j );

         /* the location to patch */
         pP = (
                 ((UChar*)(oc->image))
                 + (sectab_i->PointerToRawData
                    + reltab_j->VirtualAddress
                    - sectab_i->VirtualAddress )
              );
         /* the existing contents of pP */
         A = *(UInt32*)pP;
         /* the symbol to connect to */
         sym = (COFF_symbol*)
               myindex ( sizeof_COFF_symbol,
                         symtab, reltab_j->SymbolTableIndex );
         IF_DEBUG(linker,
                  debugBelch(
                            "reloc sec %2d num %3d:  type 0x%-4x   "
                            "vaddr 0x%-8x   name `",
                            i, j,
                            (UInt32)reltab_j->Type,
                            reltab_j->VirtualAddress );
                            printName ( sym->Name, strtab );
                            debugBelch("'\n" ));

         if (sym->StorageClass == MYIMAGE_SYM_CLASS_STATIC) {
            COFF_section* section_sym
               = findPEi386SectionCalled ( oc, sym->Name );
            if (!section_sym) {
               errorBelch("%" PATH_FMT ": can't find section `%s'", oc->fileName, sym->Name);
               return 0;
            }
            S = ((size_t)(oc->image))
              + ((size_t)(section_sym->PointerToRawData))
              + ((size_t)(sym->Value));
         } else {
            copyName ( sym->Name, strtab, symbol, 1000-1 );
            S = (size_t) lookupSymbol( (char*)symbol );
            if ((void*)S != NULL) goto foundit;
            errorBelch("%" PATH_FMT ": unknown symbol `%s'", oc->fileName, symbol);
            return 0;
           foundit:;
         }
         /* All supported relocations write at least 4 bytes */
         checkProddableBlock(oc, pP, 4);
         switch (reltab_j->Type) {
#if defined(i386_HOST_ARCH)
            case MYIMAGE_REL_I386_DIR32:
               *(UInt32 *)pP = ((UInt32)S) + A;
               break;
            case MYIMAGE_REL_I386_REL32:
               /* Tricky.  We have to insert a displacement at
                  pP which, when added to the PC for the _next_
                  insn, gives the address of the target (S).
                  Problem is to know the address of the next insn
                  when we only know pP.  We assume that this
                  literal field is always the last in the insn,
                  so that the address of the next insn is pP+4
                  -- hence the constant 4.
                  Also I don't know if A should be added, but so
                  far it has always been zero.

                  SOF 05/2005: 'A' (old contents of *pP) have been observed
                  to contain values other than zero (the 'wx' object file
                  that came with wxhaskell-0.9.4; dunno how it was compiled..).
                  So, add displacement to old value instead of asserting
                  A to be zero. Fixes wxhaskell-related crashes, and no other
                  ill effects have been observed.

                  Update: the reason why we're seeing these more elaborate
                  relocations is due to a switch in how the NCG compiles SRTs
                  and offsets to them from info tables. SRTs live in .(ro)data,
                  while info tables live in .text, causing GAS to emit REL32/DISP32
                  relocations with non-zero values. Adding the displacement is
                  the right thing to do.
               */
               *(UInt32 *)pP = ((UInt32)S) + A - ((UInt32)(size_t)pP) - 4;
               break;
#elif defined(x86_64_HOST_ARCH)
            case 2:  /* R_X86_64_32 */
            case 17: /* R_X86_64_32S */
               {
                   size_t v;
                   v = S + ((size_t)A);
                   if (v >> 32) {
                       copyName ( sym->Name, strtab, symbol, 1000-1 );
                       barf("R_X86_64_32[S]: High bits are set in %zx for %s",
                            v, (char *)symbol);
                   }
                   *(UInt32 *)pP = (UInt32)v;
                   break;
               }
            case 4: /* R_X86_64_PC32 */
               {
                   intptr_t v;
                   v = ((intptr_t)S) + ((intptr_t)(Int32)A) - ((intptr_t)pP) - 4;
                   if ((v >> 32) && ((-v) >> 32)) {
                       copyName ( sym->Name, strtab, symbol, 1000-1 );
                       barf("R_X86_64_PC32: High bits are set in %zx for %s",
                            v, (char *)symbol);
                   }
                   *(UInt32 *)pP = (UInt32)v;
                   break;
               }
            case 1: /* R_X86_64_64 */
               {
                 UInt64 A;
                 checkProddableBlock(oc, pP, 8);
                 A = *(UInt64*)pP;
                 *(UInt64 *)pP = ((UInt64)S) + ((UInt64)A);
                 break;
               }
#endif
            default:
               debugBelch("%" PATH_FMT ": unhandled PEi386 relocation type %d",
                     oc->fileName, reltab_j->Type);
               return 0;
         }

      }
   }

   IF_DEBUG(linker, debugBelch("completed %" PATH_FMT, oc->fileName));
   return 1;
}

static int
ocRunInit_PEi386 ( ObjectCode *oc )
{
    COFF_header*  hdr;
    COFF_section* sectab;
    UChar*        strtab;
    int i;

    hdr = (COFF_header*)(oc->image);
    sectab = (COFF_section*) (
                ((UChar*)(oc->image))
                + sizeof_COFF_header + hdr->SizeOfOptionalHeader
             );
    strtab = ((UChar*)(oc->image))
             + hdr->PointerToSymbolTable
             + hdr->NumberOfSymbols * sizeof_COFF_symbol;

    int argc, envc;
    char **argv, **envv;

    getProgArgv(&argc, &argv);
    getProgEnvv(&envc, &envv);

    for (i = 0; i < hdr->NumberOfSections; i++) {
        COFF_section* sectab_i
            = (COFF_section*)
                myindex ( sizeof_COFF_section, sectab, i );
        char *secname = cstring_from_section_name(sectab_i->Name, strtab);
        if (0 == strcmp(".ctors", (char*)secname)) {
            UChar *init_startC = (UChar*)(oc->image) + sectab_i->PointerToRawData;
            init_t *init_start, *init_end, *init;
            init_start = (init_t*)init_startC;
            init_end = (init_t*)(init_startC + sectab_i->SizeOfRawData);
            // ctors are run *backwards*!
            for (init = init_end - 1; init >= init_start; init--) {
                (*init)(argc, argv, envv);
            }
        }
    }
    freeProgEnvv(envc, envv);
    return 1;
}

#endif /* defined(OBJFORMAT_PEi386) */


/* --------------------------------------------------------------------------
 * ELF specifics
 * ------------------------------------------------------------------------*/

#if defined(OBJFORMAT_ELF)

#define FALSE 0
#define TRUE  1

#if defined(sparc_HOST_ARCH)
#  define ELF_TARGET_SPARC  /* Used inside <elf.h> */
#elif defined(i386_HOST_ARCH)
#  define ELF_TARGET_386    /* Used inside <elf.h> */
#elif defined(x86_64_HOST_ARCH)
#  define ELF_TARGET_X64_64
#  define ELF_64BIT
#  define ELF_TARGET_AMD64 /* Used inside <elf.h> on Solaris 11 */
#elif defined(powerpc64_HOST_ARCH)
#  define ELF_64BIT
#endif

#if !defined(openbsd_HOST_OS)
#  include <elf.h>
#else
/* openbsd elf has things in different places, with diff names */
#  include <elf_abi.h>
#  include <machine/reloc.h>
#  define R_386_32    RELOC_32
#  define R_386_PC32  RELOC_PC32
#endif

/* If elf.h doesn't define it */
#  ifndef R_X86_64_PC64
#    define R_X86_64_PC64 24
#  endif

/*
 * Workaround for libc implementations (e.g. eglibc) with incomplete
 * relocation lists
 */
#ifndef R_ARM_THM_CALL
#  define R_ARM_THM_CALL      10
#endif
#ifndef R_ARM_CALL
#  define R_ARM_CALL      28
#endif
#ifndef R_ARM_JUMP24
#  define R_ARM_JUMP24      29
#endif
#ifndef R_ARM_THM_JUMP24
#  define R_ARM_THM_JUMP24      30
#endif
#ifndef R_ARM_TARGET1
#  define R_ARM_TARGET1      38
#endif
#ifndef R_ARM_MOVW_ABS_NC
#  define R_ARM_MOVW_ABS_NC      43
#endif
#ifndef R_ARM_MOVT_ABS
#  define R_ARM_MOVT_ABS      44
#endif
#ifndef R_ARM_THM_MOVW_ABS_NC
#  define R_ARM_THM_MOVW_ABS_NC   47
#endif
#ifndef R_ARM_THM_MOVT_ABS
#  define R_ARM_THM_MOVT_ABS      48
#endif
#ifndef R_ARM_THM_JUMP11
#  define R_ARM_THM_JUMP11      102
#endif
#ifndef R_ARM_THM_JUMP8
#  define R_ARM_THM_JUMP8      103
#endif

/*
 * Define a set of types which can be used for both ELF32 and ELF64
 */

#ifdef ELF_64BIT
#define ELFCLASS    ELFCLASS64
#define Elf_Addr    Elf64_Addr
#define Elf_Word    Elf64_Word
#define Elf_Sword   Elf64_Sword
#define Elf_Ehdr    Elf64_Ehdr
#define Elf_Phdr    Elf64_Phdr
#define Elf_Shdr    Elf64_Shdr
#define Elf_Sym     Elf64_Sym
#define Elf_Rel     Elf64_Rel
#define Elf_Rela    Elf64_Rela
#ifndef ELF_ST_TYPE
#define ELF_ST_TYPE ELF64_ST_TYPE
#endif
#ifndef ELF_ST_BIND
#define ELF_ST_BIND ELF64_ST_BIND
#endif
#ifndef ELF_R_TYPE
#define ELF_R_TYPE  ELF64_R_TYPE
#endif
#ifndef ELF_R_SYM
#define ELF_R_SYM   ELF64_R_SYM
#endif
#else
#define ELFCLASS    ELFCLASS32
#define Elf_Addr    Elf32_Addr
#define Elf_Word    Elf32_Word
#define Elf_Sword   Elf32_Sword
#define Elf_Ehdr    Elf32_Ehdr
#define Elf_Phdr    Elf32_Phdr
#define Elf_Shdr    Elf32_Shdr
#define Elf_Sym     Elf32_Sym
#define Elf_Rel     Elf32_Rel
#define Elf_Rela    Elf32_Rela
#ifndef ELF_ST_TYPE
#define ELF_ST_TYPE ELF32_ST_TYPE
#endif
#ifndef ELF_ST_BIND
#define ELF_ST_BIND ELF32_ST_BIND
#endif
#ifndef ELF_R_TYPE
#define ELF_R_TYPE  ELF32_R_TYPE
#endif
#ifndef ELF_R_SYM
#define ELF_R_SYM   ELF32_R_SYM
#endif
#endif


/*
 * Functions to allocate entries in dynamic sections.  Currently we simply
 * preallocate a large number, and we don't check if a entry for the given
 * target already exists (a linear search is too slow).  Ideally these
 * entries would be associated with symbols.
 */

/* These sizes sufficient to load HSbase + HShaskell98 + a few modules */
#define GOT_SIZE            0x20000
#define FUNCTION_TABLE_SIZE 0x10000
#define PLT_SIZE            0x08000

#ifdef ELF_NEED_GOT
static Elf_Addr got[GOT_SIZE];
static unsigned int gotIndex;
static Elf_Addr gp_val = (Elf_Addr)got;

static Elf_Addr
allocateGOTEntry(Elf_Addr target)
{
   Elf_Addr *entry;

   if (gotIndex >= GOT_SIZE)
      barf("Global offset table overflow");

   entry = &got[gotIndex++];
   *entry = target;
   return (Elf_Addr)entry;
}
#endif

#ifdef ELF_FUNCTION_DESC
typedef struct {
   Elf_Addr ip;
   Elf_Addr gp;
} FunctionDesc;

static FunctionDesc functionTable[FUNCTION_TABLE_SIZE];
static unsigned int functionTableIndex;

static Elf_Addr
allocateFunctionDesc(Elf_Addr target)
{
   FunctionDesc *entry;

   if (functionTableIndex >= FUNCTION_TABLE_SIZE)
      barf("Function table overflow");

   entry = &functionTable[functionTableIndex++];
   entry->ip = target;
   entry->gp = (Elf_Addr)gp_val;
   return (Elf_Addr)entry;
}

static Elf_Addr
copyFunctionDesc(Elf_Addr target)
{
   FunctionDesc *olddesc = (FunctionDesc *)target;
   FunctionDesc *newdesc;

   newdesc = (FunctionDesc *)allocateFunctionDesc(olddesc->ip);
   newdesc->gp = olddesc->gp;
   return (Elf_Addr)newdesc;
}
#endif

#ifdef ELF_NEED_PLT

typedef struct {
   unsigned char code[sizeof(plt_code)];
} PLTEntry;

static Elf_Addr
allocatePLTEntry(Elf_Addr target, ObjectCode *oc)
{
   PLTEntry *plt = (PLTEntry *)oc->plt;
   PLTEntry *entry;

   if (oc->pltIndex >= PLT_SIZE)
      barf("Procedure table overflow");

   entry = &plt[oc->pltIndex++];
   memcpy(entry->code, plt_code, sizeof(entry->code));
   PLT_RELOC(entry->code, target);
   return (Elf_Addr)entry;
}

static unsigned int
PLTSize(void)
{
   return (PLT_SIZE * sizeof(PLTEntry));
}
#endif


/*
 * Generic ELF functions
 */

static int
ocVerifyImage_ELF ( ObjectCode* oc )
{
   Elf_Shdr* shdr;
   Elf_Sym*  stab;
   int i, j, nent, nstrtab, nsymtabs;
   char* sh_strtab;

   char*     ehdrC = (char*)(oc->image);
   Elf_Ehdr* ehdr  = (Elf_Ehdr*)ehdrC;

   if (ehdr->e_ident[EI_MAG0] != ELFMAG0 ||
       ehdr->e_ident[EI_MAG1] != ELFMAG1 ||
       ehdr->e_ident[EI_MAG2] != ELFMAG2 ||
       ehdr->e_ident[EI_MAG3] != ELFMAG3) {
      errorBelch("%s: not an ELF object", oc->fileName);
      return 0;
   }

   if (ehdr->e_ident[EI_CLASS] != ELFCLASS) {
      errorBelch("%s: unsupported ELF format", oc->fileName);
      return 0;
   }

   if (ehdr->e_ident[EI_DATA] == ELFDATA2LSB) {
       IF_DEBUG(linker,debugBelch( "Is little-endian\n" ));
   } else
   if (ehdr->e_ident[EI_DATA] == ELFDATA2MSB) {
       IF_DEBUG(linker,debugBelch( "Is big-endian\n" ));
   } else {
       errorBelch("%s: unknown endiannness", oc->fileName);
       return 0;
   }

   if (ehdr->e_type != ET_REL) {
      errorBelch("%s: not a relocatable object (.o) file", oc->fileName);
      return 0;
   }
   IF_DEBUG(linker, debugBelch( "Is a relocatable object (.o) file\n" ));

   IF_DEBUG(linker,debugBelch( "Architecture is " ));
   switch (ehdr->e_machine) {
#ifdef EM_ARM
      case EM_ARM:   IF_DEBUG(linker,debugBelch( "arm" )); break;
#endif
      case EM_386:   IF_DEBUG(linker,debugBelch( "x86" )); break;
#ifdef EM_SPARC32PLUS
      case EM_SPARC32PLUS:
#endif
      case EM_SPARC: IF_DEBUG(linker,debugBelch( "sparc" )); break;
#ifdef EM_IA_64
      case EM_IA_64: IF_DEBUG(linker,debugBelch( "ia64" )); break;
#endif
      case EM_PPC:   IF_DEBUG(linker,debugBelch( "powerpc32" )); break;
#ifdef EM_X86_64
      case EM_X86_64: IF_DEBUG(linker,debugBelch( "x86_64" )); break;
#elif defined(EM_AMD64)
      case EM_AMD64: IF_DEBUG(linker,debugBelch( "amd64" )); break;
#endif
      default:       IF_DEBUG(linker,debugBelch( "unknown" ));
                     errorBelch("%s: unknown architecture (e_machine == %d)"
                                , oc->fileName, ehdr->e_machine);
                     return 0;
   }

   IF_DEBUG(linker,debugBelch(
             "\nSection header table: start %ld, n_entries %d, ent_size %d\n",
             (long)ehdr->e_shoff, ehdr->e_shnum, ehdr->e_shentsize  ));

   ASSERT (ehdr->e_shentsize == sizeof(Elf_Shdr));

   shdr = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);

   if (ehdr->e_shstrndx == SHN_UNDEF) {
      errorBelch("%s: no section header string table", oc->fileName);
      return 0;
   } else {
      IF_DEBUG(linker,debugBelch( "Section header string table is section %d\n",
                          ehdr->e_shstrndx));
      sh_strtab = ehdrC + shdr[ehdr->e_shstrndx].sh_offset;
   }

   for (i = 0; i < ehdr->e_shnum; i++) {
      IF_DEBUG(linker,debugBelch("%2d:  ", i ));
      IF_DEBUG(linker,debugBelch("type=%2d  ", (int)shdr[i].sh_type ));
      IF_DEBUG(linker,debugBelch("size=%4d  ", (int)shdr[i].sh_size ));
      IF_DEBUG(linker,debugBelch("offs=%4d  ", (int)shdr[i].sh_offset ));
      IF_DEBUG(linker,debugBelch("  (%p .. %p)  ",
               ehdrC + shdr[i].sh_offset,
                      ehdrC + shdr[i].sh_offset + shdr[i].sh_size - 1));

#define SECTION_INDEX_VALID(ndx) (ndx > SHN_UNDEF && ndx < ehdr->e_shnum)

      switch (shdr[i].sh_type) {

        case SHT_REL:
        case SHT_RELA:
          IF_DEBUG(linker,debugBelch( shdr[i].sh_type == SHT_REL ? "Rel  " : "RelA "));

          if (!SECTION_INDEX_VALID(shdr[i].sh_link)) {
            if (shdr[i].sh_link == SHN_UNDEF)
              errorBelch("\n%s: relocation section #%d has no symbol table\n"
                         "This object file has probably been fully striped. "
                         "Such files cannot be linked.\n",
                         oc->archiveMemberName ? oc->archiveMemberName : oc->fileName, i);
            else
              errorBelch("\n%s: relocation section #%d has an invalid link field (%d)\n",
                         oc->archiveMemberName ? oc->archiveMemberName : oc->fileName,
                         i, shdr[i].sh_link);
            return 0;
          }
          if (shdr[shdr[i].sh_link].sh_type != SHT_SYMTAB) {
            errorBelch("\n%s: relocation section #%d does not link to a symbol table\n",
                       oc->archiveMemberName ? oc->archiveMemberName : oc->fileName, i);
            return 0;
          }
          if (!SECTION_INDEX_VALID(shdr[i].sh_info)) {
            errorBelch("\n%s: relocation section #%d has an invalid info field (%d)\n",
                       oc->archiveMemberName ? oc->archiveMemberName : oc->fileName,
                       i, shdr[i].sh_info);
            return 0;
          }

          break;
        case SHT_SYMTAB:
          IF_DEBUG(linker,debugBelch("Sym  "));

          if (!SECTION_INDEX_VALID(shdr[i].sh_link)) {
            errorBelch("\n%s: symbol table section #%d has an invalid link field (%d)\n",
                       oc->archiveMemberName ? oc->archiveMemberName : oc->fileName,
                       i, shdr[i].sh_link);
            return 0;
          }
          if (shdr[shdr[i].sh_link].sh_type != SHT_STRTAB) {
            errorBelch("\n%s: symbol table section #%d does not link to a string table\n",
                       oc->archiveMemberName ? oc->archiveMemberName : oc->fileName, i);

            return 0;
          }
          break;
        case SHT_STRTAB: IF_DEBUG(linker,debugBelch("Str  ")); break;
        default:         IF_DEBUG(linker,debugBelch("     ")); break;
      }
      if (sh_strtab) {
          IF_DEBUG(linker,debugBelch("sname=%s\n", sh_strtab + shdr[i].sh_name ));
      }
   }

   IF_DEBUG(linker,debugBelch( "\nString tables\n" ));
   nstrtab = 0;
   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type == SHT_STRTAB
          /* Ignore the section header's string table. */
          && i != ehdr->e_shstrndx
          /* Ignore string tables named .stabstr, as they contain
             debugging info. */
          && 0 != memcmp(".stabstr", sh_strtab + shdr[i].sh_name, 8)
         ) {
         IF_DEBUG(linker,debugBelch("   section %d is a normal string table\n", i ));
         nstrtab++;
      }
   }
   if (nstrtab == 0) {
      IF_DEBUG(linker,debugBelch("   no normal string tables (potentially, but not necessarily a problem)\n"));
   }

   nsymtabs = 0;
   IF_DEBUG(linker,debugBelch( "Symbol tables\n" ));
   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type != SHT_SYMTAB) continue;
      IF_DEBUG(linker,debugBelch( "section %d is a symbol table\n", i ));
      nsymtabs++;
      stab = (Elf_Sym*) (ehdrC + shdr[i].sh_offset);
      nent = shdr[i].sh_size / sizeof(Elf_Sym);
      IF_DEBUG(linker,debugBelch( "   number of entries is apparently %d (%ld rem)\n",
               nent,
               (long)shdr[i].sh_size % sizeof(Elf_Sym)
             ));
      if (0 != shdr[i].sh_size % sizeof(Elf_Sym)) {
         errorBelch("%s: non-integral number of symbol table entries", oc->fileName);
         return 0;
      }
      for (j = 0; j < nent; j++) {
         IF_DEBUG(linker,debugBelch("   %2d  ", j ));
         IF_DEBUG(linker,debugBelch("  sec=%-5d  size=%-3d  val=%5p  ",
                             (int)stab[j].st_shndx,
                             (int)stab[j].st_size,
                             (char*)stab[j].st_value ));

         IF_DEBUG(linker,debugBelch("type=" ));
         switch (ELF_ST_TYPE(stab[j].st_info)) {
            case STT_NOTYPE:  IF_DEBUG(linker,debugBelch("notype " )); break;
            case STT_OBJECT:  IF_DEBUG(linker,debugBelch("object " )); break;
            case STT_FUNC  :  IF_DEBUG(linker,debugBelch("func   " )); break;
            case STT_SECTION: IF_DEBUG(linker,debugBelch("section" )); break;
            case STT_FILE:    IF_DEBUG(linker,debugBelch("file   " )); break;
            default:          IF_DEBUG(linker,debugBelch("?      " )); break;
         }
         IF_DEBUG(linker,debugBelch("  " ));

         IF_DEBUG(linker,debugBelch("bind=" ));
         switch (ELF_ST_BIND(stab[j].st_info)) {
            case STB_LOCAL :  IF_DEBUG(linker,debugBelch("local " )); break;
            case STB_GLOBAL:  IF_DEBUG(linker,debugBelch("global" )); break;
            case STB_WEAK  :  IF_DEBUG(linker,debugBelch("weak  " )); break;
            default:          IF_DEBUG(linker,debugBelch("?     " )); break;
         }
         IF_DEBUG(linker,debugBelch("  " ));

         IF_DEBUG(linker,debugBelch("name=%s\n",
                        ehdrC + shdr[shdr[i].sh_link].sh_offset
                              + stab[j].st_name ));
      }
   }

   if (nsymtabs == 0) {
     // Not having a symbol table is not in principle a problem.
     // When an object file has no symbols then the 'strip' program
     // typically will remove the symbol table entirely.
     IF_DEBUG(linker,debugBelch("   no symbol tables (potentially, but not necessarily a problem)\n"));
   }

   return 1;
}

static int getSectionKind_ELF( Elf_Shdr *hdr, int *is_bss )
{
    *is_bss = FALSE;

    if (hdr->sh_type == SHT_PROGBITS
        && (hdr->sh_flags & SHF_ALLOC) && (hdr->sh_flags & SHF_EXECINSTR)) {
        /* .text-style section */
        return SECTIONKIND_CODE_OR_RODATA;
    }

    if (hdr->sh_type == SHT_PROGBITS
            && (hdr->sh_flags & SHF_ALLOC) && (hdr->sh_flags & SHF_WRITE)) {
            /* .data-style section */
            return SECTIONKIND_RWDATA;
    }

    if (hdr->sh_type == SHT_PROGBITS
        && (hdr->sh_flags & SHF_ALLOC) && !(hdr->sh_flags & SHF_WRITE)) {
        /* .rodata-style section */
        return SECTIONKIND_CODE_OR_RODATA;
    }

    if (hdr->sh_type == SHT_INIT_ARRAY
        && (hdr->sh_flags & SHF_ALLOC) && (hdr->sh_flags & SHF_WRITE)) {
       /* .init_array section */
        return SECTIONKIND_INIT_ARRAY;
    }

    if (hdr->sh_type == SHT_NOBITS
        && (hdr->sh_flags & SHF_ALLOC) && (hdr->sh_flags & SHF_WRITE)) {
        /* .bss-style section */
        *is_bss = TRUE;
        return SECTIONKIND_RWDATA;
    }

    return SECTIONKIND_OTHER;
}


static int
ocGetNames_ELF ( ObjectCode* oc )
{
   int i, j, nent;
   Elf_Sym* stab;

   char*     ehdrC    = (char*)(oc->image);
   Elf_Ehdr* ehdr     = (Elf_Ehdr*)ehdrC;
   char*     strtab;
   Elf_Shdr* shdr     = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);

   ASSERT(symhash != NULL);

   for (i = 0; i < ehdr->e_shnum; i++) {
      /* Figure out what kind of section it is.  Logic derived from
         Figure 1.14 ("Special Sections") of the ELF document
         ("Portable Formats Specification, Version 1.1"). */
      int         is_bss = FALSE;
      SectionKind kind   = getSectionKind_ELF(&shdr[i], &is_bss);

      if (is_bss && shdr[i].sh_size > 0) {
         /* This is a non-empty .bss section.  Allocate zeroed space for
            it, and set its .sh_offset field such that
            ehdrC + .sh_offset == addr_of_zeroed_space.  */
         char* zspace = stgCallocBytes(1, shdr[i].sh_size,
                                       "ocGetNames_ELF(BSS)");
         shdr[i].sh_offset = ((char*)zspace) - ((char*)ehdrC);
         /*
         debugBelch("BSS section at 0x%x, size %d\n",
                         zspace, shdr[i].sh_size);
         */
      }

      /* fill in the section info */
      if (kind != SECTIONKIND_OTHER && shdr[i].sh_size > 0) {
         addProddableBlock(oc, ehdrC + shdr[i].sh_offset, shdr[i].sh_size);
         addSection(oc, kind, ehdrC + shdr[i].sh_offset,
                        ehdrC + shdr[i].sh_offset + shdr[i].sh_size - 1);
      }

      if (shdr[i].sh_type != SHT_SYMTAB) continue;

      /* copy stuff into this module's object symbol table */
      stab = (Elf_Sym*) (ehdrC + shdr[i].sh_offset);
      strtab = ehdrC + shdr[shdr[i].sh_link].sh_offset;
      nent = shdr[i].sh_size / sizeof(Elf_Sym);

      oc->n_symbols = nent;
      oc->symbols = stgMallocBytes(oc->n_symbols * sizeof(char*),
                                   "ocGetNames_ELF(oc->symbols)");

      //TODO: we ignore local symbols anyway right? So we can use the
      //      shdr[i].sh_info to get the index of the first non-local symbol
      // ie we should use j = shdr[i].sh_info
      for (j = 0; j < nent; j++) {

         char  isLocal = FALSE; /* avoids uninit-var warning */
         HsBool isWeak = HS_BOOL_FALSE;
         char* ad      = NULL;
         char* nm      = strtab + stab[j].st_name;
         int   secno   = stab[j].st_shndx;

         /* Figure out if we want to add it; if so, set ad to its
            address.  Otherwise leave ad == NULL. */

         if (secno == SHN_COMMON) {
            isLocal = FALSE;
            ad = stgCallocBytes(1, stab[j].st_size, "ocGetNames_ELF(COMMON)");
            /*
            debugBelch("COMMON symbol, size %d name %s\n",
                            stab[j].st_size, nm);
            */
            /* Pointless to do addProddableBlock() for this area,
               since the linker should never poke around in it. */
         }
         else
         if ( ( ELF_ST_BIND(stab[j].st_info)==STB_GLOBAL
                || ELF_ST_BIND(stab[j].st_info)==STB_LOCAL
                || ELF_ST_BIND(stab[j].st_info)==STB_WEAK
              )
              /* and not an undefined symbol */
              && stab[j].st_shndx != SHN_UNDEF
              /* and not in a "special section" */
              && stab[j].st_shndx < SHN_LORESERVE
              &&
              /* and it's a not a section or string table or anything silly */
              ( ELF_ST_TYPE(stab[j].st_info)==STT_FUNC ||
                ELF_ST_TYPE(stab[j].st_info)==STT_OBJECT ||
                ELF_ST_TYPE(stab[j].st_info)==STT_NOTYPE
              )
            ) {
            /* Section 0 is the undefined section, hence > and not >=. */
            ASSERT(secno > 0 && secno < ehdr->e_shnum);
            /*
            if (shdr[secno].sh_type == SHT_NOBITS) {
               debugBelch("   BSS symbol, size %d off %d name %s\n",
                               stab[j].st_size, stab[j].st_value, nm);
            }
            */
            ad = ehdrC + shdr[ secno ].sh_offset + stab[j].st_value;
            if (ELF_ST_BIND(stab[j].st_info)==STB_LOCAL) {
               isLocal = TRUE;
               isWeak = FALSE;
            } else { /* STB_GLOBAL or STB_WEAK */
#ifdef ELF_FUNCTION_DESC
               /* dlsym() and the initialisation table both give us function
                * descriptors, so to be consistent we store function descriptors
                * in the symbol table */
               if (ELF_ST_TYPE(stab[j].st_info) == STT_FUNC)
                   ad = (char *)allocateFunctionDesc((Elf_Addr)ad);
#endif
               IF_DEBUG(linker,debugBelch( "addOTabName(GLOB): %10p  %s %s\n",
                                      ad, oc->fileName, nm ));
               isLocal = FALSE;
               isWeak = (ELF_ST_BIND(stab[j].st_info)==STB_WEAK);
            }
         }

         /* And the decision is ... */

         if (ad != NULL) {
            ASSERT(nm != NULL);
            oc->symbols[j] = nm;
            /* Acquire! */
            if (isLocal) {
               /* Ignore entirely. */
            } else {
               ghciInsertSymbolTable(oc->fileName, symhash, nm, ad, isWeak, oc);
            }
         } else {
            /* Skip. */
            IF_DEBUG(linker,debugBelch( "skipping `%s'\n",
                                   strtab + stab[j].st_name ));
            /*
            debugBelch(
                    "skipping   bind = %d,  type = %d,  shndx = %d   `%s'\n",
                    (int)ELF_ST_BIND(stab[j].st_info),
                    (int)ELF_ST_TYPE(stab[j].st_info),
                    (int)stab[j].st_shndx,
                    strtab + stab[j].st_name
                   );
            */
            oc->symbols[j] = NULL;
         }

      }
   }

   return 1;
}

/* Do ELF relocations which lack an explicit addend.  All x86-linux
   and arm-linux relocations appear to be of this form. */
static int
do_Elf_Rel_relocations ( ObjectCode* oc, char* ehdrC,
                         Elf_Shdr* shdr, int shnum )
{
   int j;
   char *symbol;
   Elf_Word* targ;
   Elf_Rel*  rtab = (Elf_Rel*) (ehdrC + shdr[shnum].sh_offset);
   Elf_Sym*  stab;
   char*     strtab;
   int         nent = shdr[shnum].sh_size / sizeof(Elf_Rel);
   int target_shndx = shdr[shnum].sh_info;
   int symtab_shndx = shdr[shnum].sh_link;
   int strtab_shndx = shdr[symtab_shndx].sh_link;

   stab  = (Elf_Sym*) (ehdrC + shdr[ symtab_shndx ].sh_offset);
   strtab= (char*)    (ehdrC + shdr[ strtab_shndx ].sh_offset);
   targ  = (Elf_Word*)(ehdrC + shdr[ target_shndx ].sh_offset);
   IF_DEBUG(linker,debugBelch( "relocations for section %d using symtab %d and strtab %d\n",
                          target_shndx, symtab_shndx, strtab_shndx ));

   /* Skip sections that we're not interested in. */
   {
       int is_bss;
       SectionKind kind = getSectionKind_ELF(&shdr[target_shndx], &is_bss);
       if (kind == SECTIONKIND_OTHER) {
           IF_DEBUG(linker,debugBelch( "skipping (target section not loaded)"));
           return 1;
       }
   }

   for (j = 0; j < nent; j++) {
      Elf_Addr offset = rtab[j].r_offset;
      Elf_Addr info   = rtab[j].r_info;

      Elf_Addr  P  = ((Elf_Addr)targ) + offset;
      Elf_Word* pP = (Elf_Word*)P;
#if defined(i386_HOST_ARCH) || defined(DEBUG)
      Elf_Addr  A  = *pP;
#endif
      Elf_Addr  S;
      void*     S_tmp;
#ifdef i386_HOST_ARCH
      Elf_Addr  value;
#endif
#ifdef arm_HOST_ARCH
      int is_target_thm=0, T=0;
#endif

      IF_DEBUG(linker,debugBelch( "Rel entry %3d is raw(%6p %6p)",
                             j, (void*)offset, (void*)info ));
      if (!info) {
         IF_DEBUG(linker,debugBelch( " ZERO" ));
         S = 0;
      } else {
         Elf_Sym sym = stab[ELF_R_SYM(info)];
         /* First see if it is a local symbol. */
         if (ELF_ST_BIND(sym.st_info) == STB_LOCAL) {
            /* Yes, so we can get the address directly from the ELF symbol
               table. */
            symbol = sym.st_name==0 ? "(noname)" : strtab+sym.st_name;
            S = (Elf_Addr)
                (ehdrC + shdr[ sym.st_shndx ].sh_offset
                       + stab[ELF_R_SYM(info)].st_value);

         } else {
            symbol = strtab + sym.st_name;
            S_tmp = lookupSymbol( symbol );
            S = (Elf_Addr)S_tmp;
         }
         if (!S) {
            errorBelch("%s: unknown symbol `%s'", oc->fileName, symbol);
            return 0;
         }
         IF_DEBUG(linker,debugBelch( "`%s' resolves to %p\n", symbol, (void*)S ));

#ifdef arm_HOST_ARCH
         // Thumb instructions have bit 0 of symbol's st_value set
         is_target_thm = S & 0x1;
         T = sym.st_info & STT_FUNC && is_target_thm;

         // Make sure we clear bit 0. Strictly speaking we should have done
         // this to st_value above but I believe alignment requirements should
         // ensure that no instructions start on an odd address
         S &= ~1;
#endif
      }

      IF_DEBUG(linker,debugBelch( "Reloc: P = %p   S = %p   A = %p\n",
                             (void*)P, (void*)S, (void*)A ));
      checkProddableBlock ( oc, pP, sizeof(Elf_Word) );

#ifdef i386_HOST_ARCH
      value = S + A;
#endif

      switch (ELF_R_TYPE(info)) {
#        ifdef i386_HOST_ARCH
         case R_386_32:   *pP = value;     break;
         case R_386_PC32: *pP = value - P; break;
#        endif

#        ifdef arm_HOST_ARCH
         case R_ARM_ABS32:
         case R_ARM_TARGET1:  // Specified by Linux ARM ABI to be equivalent to ABS32
            *(Elf32_Word *)P += S;
            *(Elf32_Word *)P |= T;
            break;

         case R_ARM_REL32:
            *(Elf32_Word *)P += S;
            *(Elf32_Word *)P |= T;
            *(Elf32_Word *)P -= P;
            break;

         case R_ARM_CALL:
         case R_ARM_JUMP24:
         {
            StgWord32 *word = (StgWord32 *)P;
            StgInt32 imm = (*word & 0x00ffffff) << 2;
            StgInt32 offset;
            int overflow;

            // Sign extend 24 to 32 bits
            if (imm & 0x02000000)
               imm -= 0x04000000;
            offset = ((S + imm) | T) - P;

            overflow = offset <= (StgInt32)0xfe000000 || offset >= (StgInt32)0x02000000;

            if ((is_target_thm && ELF_R_TYPE(info) == R_ARM_JUMP24) || overflow) {
               // Generate veneer
               // The +8 below is to undo the PC-bias compensation done by the object producer
               SymbolExtra *extra = makeArmSymbolExtra(oc, ELF_R_SYM(info), S+imm+8, 0, is_target_thm);
               // The -8 below is to compensate for PC bias
               offset = (StgWord32) &extra->jumpIsland - P - 8;
               offset &= ~1; // Clear thumb indicator bit
            } else if (is_target_thm && ELF_R_TYPE(info) == R_ARM_CALL) {
               StgWord32 cond = (*word & 0xf0000000) >> 28;
               if (cond == 0xe) {
                  // Change instruction to BLX
                  *word |= 0xf0000000; // Set first nibble
                  *word = (*word & ~0x01ffffff)
                        | ((offset >> 2) & 0x00ffffff)  // imm24
                        | ((offset & 0x2) << 23);       // H
                  break;
               } else {
                  errorBelch("%s: Can't transition from ARM to Thumb when cond != 0xe\n",
                        oc->fileName);
                  return 0;
               }
            }

            offset >>= 2;
            *word = (*word & ~0x00ffffff)
                  | (offset & 0x00ffffff);
            break;
         }

         case R_ARM_MOVT_ABS:
         case R_ARM_MOVW_ABS_NC:
         {
            StgWord32 *word = (StgWord32 *)P;
            StgInt32 offset = ((*word & 0xf0000) >> 4)
                            | (*word & 0xfff);
            // Sign extend from 16 to 32 bits
            offset = (offset ^ 0x8000) - 0x8000;

            offset += S;
            if (ELF_R_TYPE(info) == R_ARM_MOVT_ABS)
               offset >>= 16;
            else
               offset |= T;

            *word = (*word & 0xfff0f000)
                  | ((offset & 0xf000) << 4)
                  | (offset & 0x0fff);
            break;
         }

         case R_ARM_THM_CALL:
         case R_ARM_THM_JUMP24:
         {
            StgWord16 *upper = (StgWord16 *)P;
            StgWord16 *lower = (StgWord16 *)(P + 2);

            int overflow;
            int to_thm = (*lower >> 12) & 1;
            int sign = (*upper >> 10) & 1;
            int j1, j2, i1, i2;

            // Decode immediate value
            j1 = (*lower >> 13) & 1; i1 = ~(j1 ^ sign) & 1;
            j2 = (*lower >> 11) & 1; i2 = ~(j2 ^ sign) & 1;
            StgInt32 imm = (sign << 24)
                         | (i1 << 23)
                         | (i2 << 22)
                         | ((*upper & 0x03ff) << 12)
                         | ((*lower & 0x07ff) << 1);

            // Sign extend 25 to 32 bits
            if (imm & 0x01000000)
               imm -= 0x02000000;

            offset = ((imm + S) | T) - P;
            overflow = offset <= (StgWord32)0xff000000 || offset >= (StgWord32)0x01000000;

            if ((!is_target_thm && ELF_R_TYPE(info) == R_ARM_THM_JUMP24) || overflow) {
               // Generate veneer
               SymbolExtra *extra = makeArmSymbolExtra(oc, ELF_R_SYM(info), S+imm+4, 1, is_target_thm);
               offset = (StgWord32) &extra->jumpIsland - P - 4;
               sign = offset >> 31;
               to_thm = 1;
            } else if (!is_target_thm && ELF_R_TYPE(info) == R_ARM_THM_CALL) {
               offset &= ~0x3;
               to_thm = 0;
            }

            // Reencode instruction
            i1 = ~(offset >> 23) & 1; j1 = sign ^ i1;
            i2 = ~(offset >> 22) & 1; j2 = sign ^ i2;
            *upper = ( (*upper & 0xf800)
                   | (sign << 10)
                   | ((offset >> 12) & 0x03ff) );
            *lower = ( (*lower & 0xd000)
                   | (j1 << 13)
                   | (to_thm << 12)
                   | (j2 << 11)
                   | ((offset >> 1) & 0x07ff) );
            break;
         }

         case R_ARM_THM_MOVT_ABS:
         case R_ARM_THM_MOVW_ABS_NC:
         {
            StgWord16 *upper = (StgWord16 *)P;
            StgWord16 *lower = (StgWord16 *)(P + 2);
            StgInt32 offset = ((*upper & 0x000f) << 12)
                            | ((*upper & 0x0400) << 1)
                            | ((*lower & 0x7000) >> 4)
                            | (*lower & 0x00ff);

            offset = (offset ^ 0x8000) - 0x8000; // Sign extend
            offset += S;
            if (ELF_R_TYPE(info) == R_ARM_THM_MOVW_ABS_NC)
                   offset |= T;
            else if (ELF_R_TYPE(info) == R_ARM_THM_MOVT_ABS)
                   offset >>= 16;

            *upper = ( (*upper & 0xfbf0)
                   | ((offset & 0xf000) >> 12)
                   | ((offset & 0x0800) >> 1) );
            *lower = ( (*lower & 0x8f00)
                   | ((offset & 0x0700) << 4)
                   | (offset & 0x00ff) );
            break;
         }

         case R_ARM_THM_JUMP8:
         {
            StgWord16 *word = (StgWord16 *)P;
            StgWord offset = *word & 0x01fe;
            offset += S - P;
            if (!is_target_thm) {
               errorBelch("%s: Thumb to ARM transition with JUMP8 relocation not supported\n",
                     oc->fileName);
               return 0;
            }

            *word = (*word & ~0x01fe)
                  | (offset & 0x01fe);
            break;
         }

         case R_ARM_THM_JUMP11:
         {
            StgWord16 *word = (StgWord16 *)P;
            StgWord offset = *word & 0x0ffe;
            offset += S - P;
            if (!is_target_thm) {
               errorBelch("%s: Thumb to ARM transition with JUMP11 relocation not supported\n",
                     oc->fileName);
               return 0;
            }

            *word = (*word & ~0x0ffe)
                  | (offset & 0x0ffe);
            break;
         }

#        endif // arm_HOST_ARCH

         default:
            errorBelch("%s: unhandled ELF relocation(Rel) type %" FMT_Word "\n",
                  oc->fileName, (W_)ELF_R_TYPE(info));
            return 0;
      }

   }
   return 1;
}

/* Do ELF relocations for which explicit addends are supplied.
   sparc-solaris relocations appear to be of this form. */
static int
do_Elf_Rela_relocations ( ObjectCode* oc, char* ehdrC,
                          Elf_Shdr* shdr, int shnum )
{
   int j;
   char *symbol = NULL;
   Elf_Rela* rtab = (Elf_Rela*) (ehdrC + shdr[shnum].sh_offset);
   Elf_Sym*  stab;
   char*     strtab;
   int         nent = shdr[shnum].sh_size / sizeof(Elf_Rela);
   int symtab_shndx = shdr[shnum].sh_link;
   int strtab_shndx = shdr[symtab_shndx].sh_link;
#if defined(DEBUG) || defined(sparc_HOST_ARCH) || defined(ia64_HOST_ARCH) || defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
   /* This #ifdef only serves to avoid unused-var warnings. */
   Elf_Addr targ;
   int target_shndx = shdr[shnum].sh_info;
#endif

   stab  = (Elf_Sym*) (ehdrC + shdr[ symtab_shndx ].sh_offset);
   strtab= (char*)    (ehdrC + shdr[ strtab_shndx ].sh_offset);
#if defined(DEBUG) || defined(sparc_HOST_ARCH) || defined(ia64_HOST_ARCH) || defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
   /* This #ifdef only serves to avoid set-but-not-used warnings */
   targ  = (Elf_Addr) (ehdrC + shdr[ target_shndx ].sh_offset);
#endif
   IF_DEBUG(linker,debugBelch( "relocations for section %d using symtab %d\n",
                          target_shndx, symtab_shndx ));

   for (j = 0; j < nent; j++) {
#if defined(DEBUG) || defined(sparc_HOST_ARCH) || defined(ia64_HOST_ARCH) || defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
      /* This #ifdef only serves to avoid unused-var warnings. */
      Elf_Addr  offset = rtab[j].r_offset;
      Elf_Addr  P      = targ + offset;
      Elf_Addr  A      = rtab[j].r_addend;
#endif
#if defined(sparc_HOST_ARCH) || defined(ia64_HOST_ARCH) || defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
      Elf_Addr  value;
#endif
      Elf_Addr  info   = rtab[j].r_info;
      Elf_Addr  S;
      void*     S_tmp;
#     if defined(sparc_HOST_ARCH)
      Elf_Word* pP = (Elf_Word*)P;
      Elf_Word  w1, w2;
#     elif defined(powerpc_HOST_ARCH)
      Elf_Sword delta;
#     endif

      IF_DEBUG(linker,debugBelch( "Rel entry %3d is raw(%6p %6p %6p)   ",
                             j, (void*)offset, (void*)info,
                                (void*)A ));
      if (!info) {
         IF_DEBUG(linker,debugBelch( " ZERO" ));
         S = 0;
      } else {
         Elf_Sym sym = stab[ELF_R_SYM(info)];
         /* First see if it is a local symbol. */
         if (ELF_ST_BIND(sym.st_info) == STB_LOCAL) {
            /* Yes, so we can get the address directly from the ELF symbol
               table. */
            symbol = sym.st_name==0 ? "(noname)" : strtab+sym.st_name;
            S = (Elf_Addr)
                (ehdrC + shdr[ sym.st_shndx ].sh_offset
                       + stab[ELF_R_SYM(info)].st_value);
#ifdef ELF_FUNCTION_DESC
            /* Make a function descriptor for this function */
            if (S && ELF_ST_TYPE(sym.st_info) == STT_FUNC) {
               S = allocateFunctionDesc(S + A);
               A = 0;
            }
#endif
         } else {
            /* No, so look up the name in our global table. */
            symbol = strtab + sym.st_name;
            S_tmp = lookupSymbol( symbol );
            S = (Elf_Addr)S_tmp;

#ifdef ELF_FUNCTION_DESC
            /* If a function, already a function descriptor - we would
               have to copy it to add an offset. */
            if (S && (ELF_ST_TYPE(sym.st_info) == STT_FUNC) && (A != 0))
               errorBelch("%s: function %s with addend %p", oc->fileName, symbol, (void *)A);
#endif
         }
         if (!S) {
           errorBelch("%s: unknown symbol `%s'", oc->fileName, symbol);
           return 0;
         }
         IF_DEBUG(linker,debugBelch( "`%s' resolves to %p", symbol, (void*)S ));
      }

      IF_DEBUG(linker,debugBelch("Reloc: P = %p   S = %p   A = %p\n",
                                        (void*)P, (void*)S, (void*)A ));
      /* checkProddableBlock ( oc, (void*)P ); */

#if defined(sparc_HOST_ARCH) || defined(ia64_HOST_ARCH) || defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
      value = S + A;
#endif

      switch (ELF_R_TYPE(info)) {
#        if defined(sparc_HOST_ARCH)
         case R_SPARC_WDISP30:
            w1 = *pP & 0xC0000000;
            w2 = (Elf_Word)((value - P) >> 2);
            ASSERT((w2 & 0xC0000000) == 0);
            w1 |= w2;
            *pP = w1;
            break;
         case R_SPARC_HI22:
            w1 = *pP & 0xFFC00000;
            w2 = (Elf_Word)(value >> 10);
            ASSERT((w2 & 0xFFC00000) == 0);
            w1 |= w2;
            *pP = w1;
            break;
         case R_SPARC_LO10:
            w1 = *pP & ~0x3FF;
            w2 = (Elf_Word)(value & 0x3FF);
            ASSERT((w2 & ~0x3FF) == 0);
            w1 |= w2;
            *pP = w1;
            break;

         /* According to the Sun documentation:
            R_SPARC_UA32
            This relocation type resembles R_SPARC_32, except it refers to an
            unaligned word. That is, the word to be relocated must be treated
            as four separate bytes with arbitrary alignment, not as a word
            aligned according to the architecture requirements.
         */
         case R_SPARC_UA32:
            w2  = (Elf_Word)value;

            // SPARC doesn't do misaligned writes of 32 bit words,
            //       so we have to do this one byte-at-a-time.
            char *pPc   = (char*)pP;
            pPc[0]      = (char) ((Elf_Word)(w2 & 0xff000000) >> 24);
            pPc[1]      = (char) ((Elf_Word)(w2 & 0x00ff0000) >> 16);
            pPc[2]      = (char) ((Elf_Word)(w2 & 0x0000ff00) >> 8);
            pPc[3]      = (char) ((Elf_Word)(w2 & 0x000000ff));
            break;

         case R_SPARC_32:
            w2 = (Elf_Word)value;
            *pP = w2;
            break;
#        elif defined(powerpc_HOST_ARCH)
         case R_PPC_ADDR16_LO:
            *(Elf32_Half*) P = value;
            break;

         case R_PPC_ADDR16_HI:
            *(Elf32_Half*) P = value >> 16;
            break;

         case R_PPC_ADDR16_HA:
            *(Elf32_Half*) P = (value + 0x8000) >> 16;
            break;

         case R_PPC_ADDR32:
            *(Elf32_Word *) P = value;
            break;

         case R_PPC_REL32:
            *(Elf32_Word *) P = value - P;
            break;

         case R_PPC_REL24:
            delta = value - P;

            if( delta << 6 >> 6 != delta )
            {
               value = (Elf_Addr) (&makeSymbolExtra( oc, ELF_R_SYM(info), value )
                                        ->jumpIsland);
               delta = value - P;

               if( value == 0 || delta << 6 >> 6 != delta )
               {
                  barf( "Unable to make SymbolExtra for #%d",
                        ELF_R_SYM(info) );
                  return 0;
               }
            }

            *(Elf_Word *) P = (*(Elf_Word *) P & 0xfc000003)
                                          | (delta & 0x3fffffc);
            break;
#        endif

#if x86_64_HOST_ARCH
      case R_X86_64_64:
          *(Elf64_Xword *)P = value;
          break;

      case R_X86_64_PC32:
      {
#if defined(ALWAYS_PIC)
          barf("R_X86_64_PC32 relocation, but ALWAYS_PIC.");
#else
          StgInt64 off = value - P;
          if (off >= 0x7fffffffL || off < -0x80000000L) {
#if X86_64_ELF_NONPIC_HACK
              StgInt64 pltAddress = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                                -> jumpIsland;
              off = pltAddress + A - P;
#else
              barf("R_X86_64_PC32 relocation out of range: %s = %p\nRecompile %s with -fPIC.",
                   symbol, off, oc->fileName );
#endif
          }
          *(Elf64_Word *)P = (Elf64_Word)off;
#endif
          break;
      }

      case R_X86_64_PC64:
      {
          StgInt64 off = value - P;
          *(Elf64_Word *)P = (Elf64_Word)off;
          break;
      }

      case R_X86_64_32:
#if defined(ALWAYS_PIC)
          barf("R_X86_64_32 relocation, but ALWAYS_PIC.");
#else
          if (value >= 0x7fffffffL) {
#if X86_64_ELF_NONPIC_HACK
              StgInt64 pltAddress = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                                -> jumpIsland;
              value = pltAddress + A;
#else
              barf("R_X86_64_32 relocation out of range: %s = %p\nRecompile %s with -fPIC.",
                   symbol, value, oc->fileName );
#endif
          }
          *(Elf64_Word *)P = (Elf64_Word)value;
#endif
          break;

      case R_X86_64_32S:
#if defined(ALWAYS_PIC)
          barf("R_X86_64_32S relocation, but ALWAYS_PIC.");
#else
          if ((StgInt64)value > 0x7fffffffL || (StgInt64)value < -0x80000000L) {
#if X86_64_ELF_NONPIC_HACK
              StgInt64 pltAddress = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                                -> jumpIsland;
              value = pltAddress + A;
#else
              barf("R_X86_64_32S relocation out of range: %s = %p\nRecompile %s with -fPIC.",
                   symbol, value, oc->fileName );
#endif
          }
          *(Elf64_Sword *)P = (Elf64_Sword)value;
#endif
          break;

      case R_X86_64_GOTPCREL:
      {
          StgInt64 gotAddress = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)->addr;
          StgInt64 off = gotAddress + A - P;
          *(Elf64_Word *)P = (Elf64_Word)off;
          break;
      }
#if defined(dragonfly_HOST_OS)
      case R_X86_64_GOTTPOFF:
      {
#if defined(ALWAYS_PIC)
          barf("R_X86_64_GOTTPOFF relocation, but ALWAYS_PIC.");
#else
        /* determine the offset of S to the current thread's tls
           area
           XXX: Move this to the beginning of function */
          struct tls_info ti;
          get_tls_area(0, &ti, sizeof(ti));
          /* make entry in GOT that contains said offset */
          StgInt64 gotEntry = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info),
                                         (S - (Elf64_Addr)(ti.base)))->addr;
          *(Elf64_Word *)P = gotEntry + A - P;
#endif
          break;
      }
#endif



      case R_X86_64_PLT32:
      {
#if defined(ALWAYS_PIC)
          barf("R_X86_64_PLT32 relocation, but ALWAYS_PIC.");
#else
          StgInt64 off = value - P;
          if (off >= 0x7fffffffL || off < -0x80000000L) {
              StgInt64 pltAddress = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                                    -> jumpIsland;
              off = pltAddress + A - P;
          }
          *(Elf64_Word *)P = (Elf64_Word)off;
#endif
          break;
      }
#endif

         default:
            errorBelch("%s: unhandled ELF relocation(RelA) type %" FMT_Word "\n",
                  oc->fileName, (W_)ELF_R_TYPE(info));
            return 0;
      }

   }
   return 1;
}

static int
ocResolve_ELF ( ObjectCode* oc )
{
   int   shnum, ok;
   char*     ehdrC = (char*)(oc->image);
   Elf_Ehdr* ehdr  = (Elf_Ehdr*) ehdrC;
   Elf_Shdr* shdr  = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);

   /* Process the relocation sections. */
   for (shnum = 0; shnum < ehdr->e_shnum; shnum++) {
      if (shdr[shnum].sh_type == SHT_REL) {
         ok = do_Elf_Rel_relocations ( oc, ehdrC, shdr, shnum );
         if (!ok) return ok;
      }
      else
      if (shdr[shnum].sh_type == SHT_RELA) {
         ok = do_Elf_Rela_relocations ( oc, ehdrC, shdr, shnum );
         if (!ok) return ok;
      }
   }

#if defined(powerpc_HOST_ARCH) || defined(arm_HOST_ARCH)
   ocFlushInstructionCache( oc );
#endif

   return 1;
}

static int ocRunInit_ELF( ObjectCode *oc )
{
   int   i;
   char*     ehdrC = (char*)(oc->image);
   Elf_Ehdr* ehdr  = (Elf_Ehdr*) ehdrC;
   Elf_Shdr* shdr  = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);
   char* sh_strtab = ehdrC + shdr[ehdr->e_shstrndx].sh_offset;
   int argc, envc;
   char **argv, **envv;

   getProgArgv(&argc, &argv);
   getProgEnvv(&envc, &envv);

   // XXX Apparently in some archs .init may be something
   // special!  See DL_DT_INIT_ADDRESS macro in glibc
   // as well as ELF_FUNCTION_PTR_IS_SPECIAL.  We've not handled
   // it here, please file a bug report if it affects you.
   for (i = 0; i < ehdr->e_shnum; i++) {
      init_t *init_start, *init_end, *init;
      int is_bss = FALSE;
      SectionKind kind = getSectionKind_ELF(&shdr[i], &is_bss);
      if (kind == SECTIONKIND_CODE_OR_RODATA
       && 0 == memcmp(".init", sh_strtab + shdr[i].sh_name, 5)) {
         init_t init_f = (init_t)(ehdrC + shdr[i].sh_offset);
         init_f(argc, argv, envv);
      }

      if (kind == SECTIONKIND_INIT_ARRAY) {
         char *init_startC = ehdrC + shdr[i].sh_offset;
         init_start = (init_t*)init_startC;
         init_end = (init_t*)(init_startC + shdr[i].sh_size);
         for (init = init_start; init < init_end; init++) {
            (*init)(argc, argv, envv);
         }
      }

      // XXX could be more strict and assert that it's
      // SECTIONKIND_RWDATA; but allowing RODATA seems harmless enough.
      if ((kind == SECTIONKIND_RWDATA || kind == SECTIONKIND_CODE_OR_RODATA)
       && 0 == memcmp(".ctors", sh_strtab + shdr[i].sh_name, 6)) {
         char *init_startC = ehdrC + shdr[i].sh_offset;
         init_start = (init_t*)init_startC;
         init_end = (init_t*)(init_startC + shdr[i].sh_size);
         // ctors run in reverse
         for (init = init_end - 1; init >= init_start; init--) {
            (*init)(argc, argv, envv);
         }
      }
   }

   freeProgEnvv(envc, envv);
   return 1;
}

/*
 * PowerPC & X86_64 ELF specifics
 */

#if defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH) || defined(arm_HOST_ARCH)

static int ocAllocateSymbolExtras_ELF( ObjectCode *oc )
{
  Elf_Ehdr *ehdr;
  Elf_Shdr* shdr;
  int i;

  ehdr = (Elf_Ehdr *) oc->image;
  shdr = (Elf_Shdr *) ( ((char *)oc->image) + ehdr->e_shoff );

  for( i = 0; i < ehdr->e_shnum; i++ )
    if( shdr[i].sh_type == SHT_SYMTAB )
      break;

  if( i == ehdr->e_shnum )
  {
    // Not having a symbol table is not in principle a problem.
    // When an object file has no symbols then the 'strip' program
    // typically will remove the symbol table entirely.
    IF_DEBUG(linker, debugBelch( "The ELF file %s contains no symtab\n",
             oc->archiveMemberName ? oc->archiveMemberName : oc->fileName ));
    return 1;
  }

  if( shdr[i].sh_entsize != sizeof( Elf_Sym ) )
  {
    errorBelch( "The entry size (%d) of the symtab isn't %d\n",
      (int) shdr[i].sh_entsize, (int) sizeof( Elf_Sym ) );

    return 0;
  }

  return ocAllocateSymbolExtras( oc, shdr[i].sh_size / sizeof( Elf_Sym ), 0 );
}

#endif /* powerpc */

#endif /* ELF */

/* --------------------------------------------------------------------------
 * Mach-O specifics
 * ------------------------------------------------------------------------*/

#if defined(OBJFORMAT_MACHO)

/*
  Support for MachO linking on Darwin/MacOS X
  by Wolfgang Thaller (wolfgang.thaller@gmx.net)

  I hereby formally apologize for the hackish nature of this code.
  Things that need to be done:
  *) implement ocVerifyImage_MachO
  *) add still more sanity checks.
*/

#if x86_64_HOST_ARCH || powerpc64_HOST_ARCH
#define mach_header mach_header_64
#define segment_command segment_command_64
#define section section_64
#define nlist nlist_64
#endif

#ifdef powerpc_HOST_ARCH
static int
ocAllocateSymbolExtras_MachO(ObjectCode* oc)
{
    struct mach_header *header = (struct mach_header *) oc->image;
    struct load_command *lc = (struct load_command *) (header + 1);
    unsigned i;

    IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: start\n"));

    for (i = 0; i < header->ncmds; i++) {
        if (lc->cmd == LC_SYMTAB) {

                // Find out the first and last undefined external
                // symbol, so we don't have to allocate too many
            // jump islands/GOT entries.

            struct symtab_command *symLC = (struct symtab_command *) lc;
            unsigned min = symLC->nsyms, max = 0;
            struct nlist *nlist =
                symLC ? (struct nlist*) ((char*) oc->image + symLC->symoff)
                      : NULL;

            for (i = 0; i < symLC->nsyms; i++) {

                if (nlist[i].n_type & N_STAB) {
                    ;
                } else if (nlist[i].n_type & N_EXT) {

                    if((nlist[i].n_type & N_TYPE) == N_UNDF
                        && (nlist[i].n_value == 0)) {

                        if (i < min) {
                            min = i;
                        }

                        if (i > max) {
                            max = i;
                    }
                }
            }
            }

            if (max >= min) {
                return ocAllocateSymbolExtras(oc, max - min + 1, min);
            }

            break;
        }

        lc = (struct load_command *) ( ((char *)lc) + lc->cmdsize );
    }

    return ocAllocateSymbolExtras(oc,0,0);
}

#endif
#ifdef x86_64_HOST_ARCH
static int
ocAllocateSymbolExtras_MachO(ObjectCode* oc)
{
    struct mach_header *header = (struct mach_header *) oc->image;
    struct load_command *lc = (struct load_command *) (header + 1);
    unsigned i;

    IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: start\n"));

    for (i = 0; i < header->ncmds; i++) {
        if (lc->cmd == LC_SYMTAB) {

                // Just allocate one entry for every symbol
            struct symtab_command *symLC = (struct symtab_command *) lc;

            IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: allocate %d symbols\n", symLC->nsyms));
            IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: done\n"));
            return ocAllocateSymbolExtras(oc, symLC->nsyms, 0);
        }

        lc = (struct load_command *) ( ((char *)lc) + lc->cmdsize );
    }

    IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: allocated no symbols\n"));
    IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: done\n"));
    return ocAllocateSymbolExtras(oc,0,0);
}
#endif

static int
ocVerifyImage_MachO(ObjectCode * oc)
{
    char *image = (char*) oc->image;
    struct mach_header *header = (struct mach_header*) image;

    IF_DEBUG(linker, debugBelch("ocVerifyImage_MachO: start\n"));

#if x86_64_HOST_ARCH || powerpc64_HOST_ARCH
    if(header->magic != MH_MAGIC_64) {
        errorBelch("Could not load image %s: bad magic!\n"
                   "  Expected %08x (64bit), got %08x%s\n",
                   oc->fileName, MH_MAGIC_64, header->magic,
                   header->magic == MH_MAGIC ? " (32bit)." : ".");
        return 0;
    }
#else
    if(header->magic != MH_MAGIC) {
        errorBelch("Could not load image %s: bad magic!\n"
                   "  Expected %08x (32bit), got %08x%s\n",
                   oc->fileName, MH_MAGIC, header->magic,
                   header->magic == MH_MAGIC_64 ? " (64bit)." : ".");
        return 0;
    }
#endif

    // FIXME: do some more verifying here
    IF_DEBUG(linker, debugBelch("ocVerifyImage_MachO: done\n"));
    return 1;
}

static int
resolveImports(
    ObjectCode* oc,
    char *image,
    struct symtab_command *symLC,
    struct section *sect,    // ptr to lazy or non-lazy symbol pointer section
    unsigned long *indirectSyms,
    struct nlist *nlist)
{
    unsigned i;
    size_t itemSize = 4;

    IF_DEBUG(linker, debugBelch("resolveImports: start\n"));

#if i386_HOST_ARCH
    int isJumpTable = 0;

    if (strcmp(sect->sectname,"__jump_table") == 0) {
        isJumpTable = 1;
        itemSize = 5;
        ASSERT(sect->reserved2 == itemSize);
    }

#endif

    for(i = 0; i * itemSize < sect->size; i++)
    {
        // according to otool, reserved1 contains the first index into the indirect symbol table
        struct nlist *symbol = &nlist[indirectSyms[sect->reserved1+i]];
        char *nm = image + symLC->stroff + symbol->n_un.n_strx;
        void *addr = NULL;

        IF_DEBUG(linker, debugBelch("resolveImports: resolving %s\n", nm));

        if ((symbol->n_type & N_TYPE) == N_UNDF
            && (symbol->n_type & N_EXT) && (symbol->n_value != 0)) {
            addr = (void*) (symbol->n_value);
            IF_DEBUG(linker, debugBelch("resolveImports: undefined external %s has value %p\n", nm, addr));
        } else {
            addr = lookupSymbol(nm);
            IF_DEBUG(linker, debugBelch("resolveImports: looking up %s, %p\n", nm, addr));
        }

        if (addr == NULL)
        {
            errorBelch("\nlookupSymbol failed in resolveImports\n"
                       "%s: unknown symbol `%s'", oc->fileName, nm);
            return 0;
        }
        ASSERT(addr);

#if i386_HOST_ARCH
        if (isJumpTable) {
            checkProddableBlock(oc,image + sect->offset + i*itemSize, 5);

            *(image + sect->offset + i * itemSize) = 0xe9; // jmp opcode
            *(unsigned*)(image + sect->offset + i*itemSize + 1)
                = (char*)addr - (image + sect->offset + i*itemSize + 5);
        }
        else
#endif
        {
            checkProddableBlock(oc,
                                ((void**)(image + sect->offset)) + i,
                                sizeof(void *));
            ((void**)(image + sect->offset))[i] = addr;
        }
    }

    IF_DEBUG(linker, debugBelch("resolveImports: done\n"));
    return 1;
}

static unsigned long
relocateAddress(
    ObjectCode* oc,
    int nSections,
    struct section* sections,
    unsigned long address)
{
    int i;
    IF_DEBUG(linker, debugBelch("relocateAddress: start\n"));
    for (i = 0; i < nSections; i++)
    {
            IF_DEBUG(linker, debugBelch("    relocating address in section %d\n", i));
        if (sections[i].addr <= address
            && address < sections[i].addr + sections[i].size)
        {
            return (unsigned long)oc->image
                    + sections[i].offset + address - sections[i].addr;
        }
    }
    barf("Invalid Mach-O file:"
         "Address out of bounds while relocating object file");
    return 0;
}

static int
relocateSection(
    ObjectCode* oc,
    char *image,
    struct symtab_command *symLC, struct nlist *nlist,
    int nSections, struct section* sections, struct section *sect)
{
    struct relocation_info *relocs;
    int i, n;

    IF_DEBUG(linker, debugBelch("relocateSection: start\n"));

    if(!strcmp(sect->sectname,"__la_symbol_ptr"))
        return 1;
    else if(!strcmp(sect->sectname,"__nl_symbol_ptr"))
        return 1;
    else if(!strcmp(sect->sectname,"__la_sym_ptr2"))
        return 1;
    else if(!strcmp(sect->sectname,"__la_sym_ptr3"))
        return 1;

    n = sect->nreloc;
    IF_DEBUG(linker, debugBelch("relocateSection: number of relocations: %d\n", n));

    relocs = (struct relocation_info*) (image + sect->reloff);

    for(i = 0; i < n; i++)
    {
#ifdef x86_64_HOST_ARCH
        struct relocation_info *reloc = &relocs[i];

        char    *thingPtr = image + sect->offset + reloc->r_address;
        uint64_t thing;
        /* We shouldn't need to initialise this, but gcc on OS X 64 bit
           complains that it may be used uninitialized if we don't */
        uint64_t value = 0;
        uint64_t baseValue;
        int type = reloc->r_type;

        IF_DEBUG(linker, debugBelch("relocateSection: relocation %d\n", i));
        IF_DEBUG(linker, debugBelch("               : type      = %d\n", reloc->r_type));
        IF_DEBUG(linker, debugBelch("               : address   = %d\n", reloc->r_address));
        IF_DEBUG(linker, debugBelch("               : symbolnum = %u\n", reloc->r_symbolnum));
        IF_DEBUG(linker, debugBelch("               : pcrel     = %d\n", reloc->r_pcrel));
        IF_DEBUG(linker, debugBelch("               : length    = %d\n", reloc->r_length));
        IF_DEBUG(linker, debugBelch("               : extern    = %d\n", reloc->r_extern));
        IF_DEBUG(linker, debugBelch("               : type      = %d\n", reloc->r_type));

        switch(reloc->r_length)
        {
            case 0:
                checkProddableBlock(oc,thingPtr,1);
                thing = *(uint8_t*)thingPtr;
                baseValue = (uint64_t)thingPtr + 1;
                break;
            case 1:
                checkProddableBlock(oc,thingPtr,2);
                thing = *(uint16_t*)thingPtr;
                baseValue = (uint64_t)thingPtr + 2;
                break;
            case 2:
                checkProddableBlock(oc,thingPtr,4);
                thing = *(uint32_t*)thingPtr;
                baseValue = (uint64_t)thingPtr + 4;
                break;
            case 3:
                checkProddableBlock(oc,thingPtr,8);
                thing = *(uint64_t*)thingPtr;
                baseValue = (uint64_t)thingPtr + 8;
                break;
            default:
                barf("Unknown size.");
        }

        IF_DEBUG(linker,
                 debugBelch("relocateSection: length = %d, thing = %" PRId64 ", baseValue = %p\n",
                            reloc->r_length, thing, (char *)baseValue));

        if (type == X86_64_RELOC_GOT
         || type == X86_64_RELOC_GOT_LOAD)
        {
            struct nlist *symbol = &nlist[reloc->r_symbolnum];
            char *nm = image + symLC->stroff + symbol->n_un.n_strx;
            void *addr = NULL;

            IF_DEBUG(linker, debugBelch("relocateSection: making jump island for %s, extern = %d, X86_64_RELOC_GOT\n", nm, reloc->r_extern));

            ASSERT(reloc->r_extern);
            if (reloc->r_extern == 0) {
                    errorBelch("\nrelocateSection: global offset table relocation for symbol with r_extern == 0\n");
            }

            if (symbol->n_type & N_EXT) {
                    // The external bit is set, meaning the symbol is exported,
                    // and therefore can be looked up in this object module's
                    // symtab, or it is undefined, meaning dlsym must be used
                    // to resolve it.

                    addr = lookupSymbol(nm);
                    IF_DEBUG(linker, debugBelch("relocateSection: looked up %s, "
                                                "external X86_64_RELOC_GOT or X86_64_RELOC_GOT_LOAD\n", nm));
                    IF_DEBUG(linker, debugBelch("               : addr = %p\n", addr));

                    if (addr == NULL) {
                            errorBelch("\nlookupSymbol failed in relocateSection (RELOC_GOT)\n"
                                       "%s: unknown symbol `%s'", oc->fileName, nm);
                            return 0;
                    }
            } else {
                    IF_DEBUG(linker, debugBelch("relocateSection: %s is not an exported symbol\n", nm));

                    // The symbol is not exported, or defined in another
                    // module, so it must be in the current object module,
                    // at the location given by the section index and
                    // symbol address (symbol->n_value)

                    if ((symbol->n_type & N_TYPE) == N_SECT) {
                            addr = (void *)relocateAddress(oc, nSections, sections, symbol->n_value);
                            IF_DEBUG(linker, debugBelch("relocateSection: calculated relocation %p of "
                                                        "non-external X86_64_RELOC_GOT or X86_64_RELOC_GOT_LOAD\n",
                                                        (void *)symbol->n_value));
                            IF_DEBUG(linker, debugBelch("               : addr = %p\n", addr));
                    } else {
                            errorBelch("\nrelocateSection: %s is not exported,"
                                       " and should be defined in a section, but isn't!\n", nm);
                    }
            }

            value = (uint64_t) &makeSymbolExtra(oc, reloc->r_symbolnum, (unsigned long)addr)->addr;

            type = X86_64_RELOC_SIGNED;
        }
        else if (reloc->r_extern)
        {
            struct nlist *symbol = &nlist[reloc->r_symbolnum];
            char *nm = image + symLC->stroff + symbol->n_un.n_strx;
            void *addr = NULL;

            IF_DEBUG(linker, debugBelch("relocateSection: looking up external symbol %s\n", nm));
            IF_DEBUG(linker, debugBelch("               : type  = %d\n", symbol->n_type));
            IF_DEBUG(linker, debugBelch("               : sect  = %d\n", symbol->n_sect));
            IF_DEBUG(linker, debugBelch("               : desc  = %d\n", symbol->n_desc));
            IF_DEBUG(linker, debugBelch("               : value = %p\n", (void *)symbol->n_value));

            if ((symbol->n_type & N_TYPE) == N_SECT) {
                value = relocateAddress(oc, nSections, sections,
                                        symbol->n_value);
                IF_DEBUG(linker, debugBelch("relocateSection, defined external symbol %s, relocated address %p\n", nm, (void *)value));
            }
            else {
                addr = lookupSymbol(nm);
                if (addr == NULL)
                {
                     errorBelch("\nlookupSymbol failed in relocateSection (relocate external)\n"
                                "%s: unknown symbol `%s'", oc->fileName, nm);
                     return 0;
                }

                value = (uint64_t) addr;
                IF_DEBUG(linker, debugBelch("relocateSection: external symbol %s, address %p\n", nm, (void *)value));
            }
        }
        else
        {
            // If the relocation is not through the global offset table
            // or external, then set the value to the baseValue.  This
            // will leave displacements into the __const section
            // unchanged (as they ought to be).

            value = baseValue;
        }

        IF_DEBUG(linker, debugBelch("relocateSection: value = %p\n", (void *)value));

        if (type == X86_64_RELOC_BRANCH)
        {
            if((int32_t)(value - baseValue) != (int64_t)(value - baseValue))
            {
                ASSERT(reloc->r_extern);
                value = (uint64_t) &makeSymbolExtra(oc, reloc->r_symbolnum, value)
                                        -> jumpIsland;
            }
            ASSERT((int32_t)(value - baseValue) == (int64_t)(value - baseValue));
            type = X86_64_RELOC_SIGNED;
        }

        switch(type)
        {
            case X86_64_RELOC_UNSIGNED:
                ASSERT(!reloc->r_pcrel);
                thing += value;
                break;
            case X86_64_RELOC_SIGNED:
            case X86_64_RELOC_SIGNED_1:
            case X86_64_RELOC_SIGNED_2:
            case X86_64_RELOC_SIGNED_4:
                ASSERT(reloc->r_pcrel);
                thing += value - baseValue;
                break;
            case X86_64_RELOC_SUBTRACTOR:
                ASSERT(!reloc->r_pcrel);
                thing -= value;
                break;
            default:
                barf("unkown relocation");
        }

        switch(reloc->r_length)
        {
            case 0:
                *(uint8_t*)thingPtr = thing;
                break;
            case 1:
                *(uint16_t*)thingPtr = thing;
                break;
            case 2:
                *(uint32_t*)thingPtr = thing;
                break;
            case 3:
                *(uint64_t*)thingPtr = thing;
                break;
        }
#else
        if(relocs[i].r_address & R_SCATTERED)
        {
            struct scattered_relocation_info *scat =
                (struct scattered_relocation_info*) &relocs[i];

            if(!scat->r_pcrel)
            {
                if(scat->r_length == 2)
                {
                    unsigned long word = 0;
                    unsigned long* wordPtr = (unsigned long*) (image + sect->offset + scat->r_address);

                    /* In this check we assume that sizeof(unsigned long) = 2 * sizeof(unsigned short)
                       on powerpc_HOST_ARCH */
                    checkProddableBlock(oc,wordPtr,sizeof(unsigned long));

                    // Note on relocation types:
                    // i386 uses the GENERIC_RELOC_* types,
                    // while ppc uses special PPC_RELOC_* types.
                    // *_RELOC_VANILLA and *_RELOC_PAIR have the same value
                    // in both cases, all others are different.
                    // Therefore, we use GENERIC_RELOC_VANILLA
                    // and GENERIC_RELOC_PAIR instead of the PPC variants,
                    // and use #ifdefs for the other types.

                    // Step 1: Figure out what the relocated value should be
                    if (scat->r_type == GENERIC_RELOC_VANILLA) {
                        word = *wordPtr
                             + (unsigned long) relocateAddress(oc,
                                                                nSections,
                                                                sections,
                                                                scat->r_value)
                                        - scat->r_value;
                    }
#ifdef powerpc_HOST_ARCH
                    else if(scat->r_type == PPC_RELOC_SECTDIFF
                        || scat->r_type == PPC_RELOC_LO16_SECTDIFF
                        || scat->r_type == PPC_RELOC_HI16_SECTDIFF
                        || scat->r_type == PPC_RELOC_HA16_SECTDIFF
                        || scat->r_type == PPC_RELOC_LOCAL_SECTDIFF)
#else
                    else if(scat->r_type == GENERIC_RELOC_SECTDIFF
                        || scat->r_type == GENERIC_RELOC_LOCAL_SECTDIFF)
#endif
                    {
                        struct scattered_relocation_info *pair =
                                (struct scattered_relocation_info*) &relocs[i+1];

                        if (!pair->r_scattered || pair->r_type != GENERIC_RELOC_PAIR) {
                            barf("Invalid Mach-O file: "
                                 "RELOC_*_SECTDIFF not followed by RELOC_PAIR");
                        }

                        word = (unsigned long)
                               (relocateAddress(oc, nSections, sections, scat->r_value)
                              - relocateAddress(oc, nSections, sections, pair->r_value));
                        i++;
                    }
#ifdef powerpc_HOST_ARCH
                    else if(scat->r_type == PPC_RELOC_HI16
                         || scat->r_type == PPC_RELOC_LO16
                         || scat->r_type == PPC_RELOC_HA16
                         || scat->r_type == PPC_RELOC_LO14)
                    {   // these are generated by label+offset things
                        struct relocation_info *pair = &relocs[i+1];

                        if ((pair->r_address & R_SCATTERED) || pair->r_type != PPC_RELOC_PAIR) {
                            barf("Invalid Mach-O file: "
                                 "PPC_RELOC_* not followed by PPC_RELOC_PAIR");
                        }

                        if(scat->r_type == PPC_RELOC_LO16)
                        {
                            word = ((unsigned short*) wordPtr)[1];
                            word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF) << 16;
                        }
                        else if(scat->r_type == PPC_RELOC_LO14)
                        {
                            barf("Unsupported Relocation: PPC_RELOC_LO14");
                            word = ((unsigned short*) wordPtr)[1] & 0xFFFC;
                            word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF) << 16;
                        }
                        else if(scat->r_type == PPC_RELOC_HI16)
                        {
                            word = ((unsigned short*) wordPtr)[1] << 16;
                            word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF);
                        }
                        else if(scat->r_type == PPC_RELOC_HA16)
                        {
                            word = ((unsigned short*) wordPtr)[1] << 16;
                            word += ((short)relocs[i+1].r_address & (short)0xFFFF);
                        }


                        word += (unsigned long) relocateAddress(oc, nSections, sections, scat->r_value)
                                                - scat->r_value;

                        i++;
                    }
 #endif
                    else {
                        barf ("Don't know how to handle this Mach-O "
                              "scattered relocation entry: "
                              "object file %s; entry type %ld; "
                              "address %#lx\n",
                              OC_INFORMATIVE_FILENAME(oc),
                              scat->r_type,
                              scat->r_address);
                        return 0;
                     }

#ifdef powerpc_HOST_ARCH
                    if(scat->r_type == GENERIC_RELOC_VANILLA
                        || scat->r_type == PPC_RELOC_SECTDIFF)
#else
                    if(scat->r_type == GENERIC_RELOC_VANILLA
                        || scat->r_type == GENERIC_RELOC_SECTDIFF
                        || scat->r_type == GENERIC_RELOC_LOCAL_SECTDIFF)
#endif
                    {
                        *wordPtr = word;
                    }
#ifdef powerpc_HOST_ARCH
                    else if (scat->r_type == PPC_RELOC_LO16_SECTDIFF
                          || scat->r_type == PPC_RELOC_LO16)
                    {
                        ((unsigned short*) wordPtr)[1] = word & 0xFFFF;
                    }
                    else if (scat->r_type == PPC_RELOC_HI16_SECTDIFF
                          || scat->r_type == PPC_RELOC_HI16)
                    {
                        ((unsigned short*) wordPtr)[1] = (word >> 16) & 0xFFFF;
                    }
                    else if (scat->r_type == PPC_RELOC_HA16_SECTDIFF
                          || scat->r_type == PPC_RELOC_HA16)
                    {
                        ((unsigned short*) wordPtr)[1] = ((word >> 16) & 0xFFFF)
                            + ((word & (1<<15)) ? 1 : 0);
                    }
#endif
                }
                else
                {
                    barf("Can't handle Mach-O scattered relocation entry "
                         "with this r_length tag: "
                         "object file %s; entry type %ld; "
                         "r_length tag %ld; address %#lx\n",
                         OC_INFORMATIVE_FILENAME(oc),
                         scat->r_type,
                         scat->r_length,
                         scat->r_address);
                    return 0;
                }
            }
            else /* scat->r_pcrel */
            {
                barf("Don't know how to handle *PC-relative* Mach-O "
                     "scattered relocation entry: "
                     "object file %s; entry type %ld; address %#lx\n",
                     OC_INFORMATIVE_FILENAME(oc),
                     scat->r_type,
                     scat->r_address);
               return 0;
            }

        }
        else /* !(relocs[i].r_address & R_SCATTERED) */
        {
            struct relocation_info *reloc = &relocs[i];
            if (reloc->r_pcrel && !reloc->r_extern) {
                IF_DEBUG(linker, debugBelch("relocateSection: pc relative but not external, skipping\n"));
                continue;
            }

            if (reloc->r_length == 2) {
                unsigned long word = 0;
#ifdef powerpc_HOST_ARCH
                unsigned long jumpIsland = 0;
                long offsetToJumpIsland = 0xBADBAD42; // initialise to bad value
                                                      // to avoid warning and to catch
                                                      // bugs.
#endif

                unsigned long* wordPtr = (unsigned long*) (image + sect->offset + reloc->r_address);

                /* In this check we assume that sizeof(unsigned long) = 2 * sizeof(unsigned short)
                   on powerpc_HOST_ARCH */
                checkProddableBlock(oc,wordPtr, sizeof(unsigned long));

                if (reloc->r_type == GENERIC_RELOC_VANILLA) {
                    word = *wordPtr;
                }
#ifdef powerpc_HOST_ARCH
                else if (reloc->r_type == PPC_RELOC_LO16) {
                    word = ((unsigned short*) wordPtr)[1];
                    word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF) << 16;
                }
                else if (reloc->r_type == PPC_RELOC_HI16) {
                    word = ((unsigned short*) wordPtr)[1] << 16;
                    word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF);
                }
                else if (reloc->r_type == PPC_RELOC_HA16) {
                    word = ((unsigned short*) wordPtr)[1] << 16;
                    word += ((short)relocs[i+1].r_address & (short)0xFFFF);
                }
                else if (reloc->r_type == PPC_RELOC_BR24) {
                    word = *wordPtr;
                    word = (word & 0x03FFFFFC) | ((word & 0x02000000) ? 0xFC000000 : 0);
                }
#endif
                else {
                    barf("Can't handle this Mach-O relocation entry "
                         "(not scattered): "
                         "object file %s; entry type %ld; address %#lx\n",
                         OC_INFORMATIVE_FILENAME(oc),
                         reloc->r_type,
                         reloc->r_address);
                    return 0;
                }

                if (!reloc->r_extern) {
                    long delta = sections[reloc->r_symbolnum-1].offset
                        - sections[reloc->r_symbolnum-1].addr
                        + ((long) image);

                    word += delta;
                }
                else {
                    struct nlist *symbol = &nlist[reloc->r_symbolnum];
                    char *nm = image + symLC->stroff + symbol->n_un.n_strx;
                    void *symbolAddress = lookupSymbol(nm);

                    if (!symbolAddress) {
                        errorBelch("\nunknown symbol `%s'", nm);
                        return 0;
                    }

                    if (reloc->r_pcrel) {
#ifdef powerpc_HOST_ARCH
                            // In the .o file, this should be a relative jump to NULL
                            // and we'll change it to a relative jump to the symbol
                        ASSERT(word + reloc->r_address == 0);
                        jumpIsland = (unsigned long)
                                        &makeSymbolExtra(oc,
                                                         reloc->r_symbolnum,
                                                         (unsigned long) symbolAddress)
                                         -> jumpIsland;
                        if (jumpIsland != 0) {
                            offsetToJumpIsland = word + jumpIsland
                                - (((long)image) + sect->offset - sect->addr);
                        }
#endif
                        word += (unsigned long) symbolAddress
                                - (((long)image) + sect->offset - sect->addr);
                    }
                    else {
                        word += (unsigned long) symbolAddress;
                    }
                }

                if (reloc->r_type == GENERIC_RELOC_VANILLA) {
                    *wordPtr = word;
                    continue;
                }
#ifdef powerpc_HOST_ARCH
                else if(reloc->r_type == PPC_RELOC_LO16)
                {
                    ((unsigned short*) wordPtr)[1] = word & 0xFFFF;
                    i++;
                    continue;
                }
                else if(reloc->r_type == PPC_RELOC_HI16)
                {
                    ((unsigned short*) wordPtr)[1] = (word >> 16) & 0xFFFF;
                    i++;
                    continue;
                }
                else if(reloc->r_type == PPC_RELOC_HA16)
                {
                    ((unsigned short*) wordPtr)[1] = ((word >> 16) & 0xFFFF)
                        + ((word & (1<<15)) ? 1 : 0);
                    i++;
                    continue;
                }
                else if(reloc->r_type == PPC_RELOC_BR24)
                {
                    if ((word & 0x03) != 0) {
                        barf("%s: unconditional relative branch with a displacement "
                             "which isn't a multiple of 4 bytes: %#lx",
                             OC_INFORMATIVE_FILENAME(oc),
                             word);
                    }

                    if((word & 0xFE000000) != 0xFE000000 &&
                        (word & 0xFE000000) != 0x00000000) {
                        // The branch offset is too large.
                        // Therefore, we try to use a jump island.
                        if (jumpIsland == 0) {
                            barf("%s: unconditional relative branch out of range: "
                                 "no jump island available: %#lx",
                                 OC_INFORMATIVE_FILENAME(oc),
                                 word);
                        }

                        word = offsetToJumpIsland;

                        if((word & 0xFE000000) != 0xFE000000 &&
                            (word & 0xFE000000) != 0x00000000) {
                            barf("%s: unconditional relative branch out of range: "
                                 "jump island out of range: %#lx",
                                 OC_INFORMATIVE_FILENAME(oc),
                                 word);
                    }
                    }
                    *wordPtr = (*wordPtr & 0xFC000003) | (word & 0x03FFFFFC);
                    continue;
                }
#endif
            }
            else
            {
                 barf("Can't handle Mach-O relocation entry (not scattered) "
                      "with this r_length tag: "
                      "object file %s; entry type %ld; "
                      "r_length tag %ld; address %#lx\n",
                      OC_INFORMATIVE_FILENAME(oc),
                      reloc->r_type,
                      reloc->r_length,
                      reloc->r_address);
                 return 0;
            }
        }
#endif
    }

    IF_DEBUG(linker, debugBelch("relocateSection: done\n"));
    return 1;
}

static int
ocGetNames_MachO(ObjectCode* oc)
{
    char *image = (char*) oc->image;
    struct mach_header *header = (struct mach_header*) image;
    struct load_command *lc = (struct load_command*) (image + sizeof(struct mach_header));
    unsigned i,curSymbol = 0;
    struct segment_command *segLC = NULL;
    struct section *sections;
    struct symtab_command *symLC = NULL;
    struct nlist *nlist;
    unsigned long commonSize = 0;
    char    *commonStorage = NULL;
    unsigned long commonCounter;

    IF_DEBUG(linker,debugBelch("ocGetNames_MachO: start\n"));

    for(i=0;i<header->ncmds;i++)
    {
        if (lc->cmd == LC_SEGMENT || lc->cmd == LC_SEGMENT_64) {
            segLC = (struct segment_command*) lc;
        }
        else if (lc->cmd == LC_SYMTAB) {
            symLC = (struct symtab_command*) lc;
        }

        lc = (struct load_command *) ( ((char*)lc) + lc->cmdsize );
    }

    sections = (struct section*) (segLC+1);
    nlist = symLC ? (struct nlist*) (image + symLC->symoff)
                  : NULL;

    if (!segLC) {
        barf("ocGetNames_MachO: no segment load command");
    }

    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: will load %d sections\n", segLC->nsects));
    for(i=0;i<segLC->nsects;i++)
    {
        IF_DEBUG(linker, debugBelch("ocGetNames_MachO: section %d\n", i));

        if (sections[i].size == 0) {
            IF_DEBUG(linker, debugBelch("ocGetNames_MachO: found a zero length section, skipping\n"));
            continue;
        }

        if((sections[i].flags & SECTION_TYPE) == S_ZEROFILL)
        {
#ifdef USE_MMAP
            char * zeroFillArea = mmapForLinker(sections[i].size, MAP_ANONYMOUS, -1);
            memset(zeroFillArea, 0, sections[i].size);
#else
            char * zeroFillArea = stgCallocBytes(1,sections[i].size,
                                      "ocGetNames_MachO(common symbols)");
#endif
            sections[i].offset = zeroFillArea - image;
        }

        SectionKind kind = SECTIONKIND_OTHER;

        if (0==strcmp(sections[i].sectname,"__text")) {
            kind = SECTIONKIND_CODE_OR_RODATA;
        }
        else if (0==strcmp(sections[i].sectname,"__const") ||
                 0==strcmp(sections[i].sectname,"__data") ||
                 0==strcmp(sections[i].sectname,"__bss") ||
                 0==strcmp(sections[i].sectname,"__common") ||
                 0==strcmp(sections[i].sectname,"__mod_init_func")) {
            kind = SECTIONKIND_RWDATA;
        }

        if (kind != SECTIONKIND_OTHER) {
            addSection(oc, kind,
                (void*) (image + sections[i].offset),
                (void*) (image + sections[i].offset + sections[i].size));
        }
        addProddableBlock(oc,
                          (void *) (image + sections[i].offset),
                                        sections[i].size);
    }

        // count external symbols defined here
    oc->n_symbols = 0;
    if (symLC) {
        for (i = 0; i < symLC->nsyms; i++) {
            if (nlist[i].n_type & N_STAB) {
                ;
            }
            else if(nlist[i].n_type & N_EXT)
            {
                if((nlist[i].n_type & N_TYPE) == N_UNDF
                    && (nlist[i].n_value != 0))
                {
                    commonSize += nlist[i].n_value;
                    oc->n_symbols++;
                }
                else if((nlist[i].n_type & N_TYPE) == N_SECT)
                    oc->n_symbols++;
            }
        }
    }
    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: %d external symbols\n", oc->n_symbols));
    oc->symbols = stgMallocBytes(oc->n_symbols * sizeof(char*),
                                   "ocGetNames_MachO(oc->symbols)");

    if(symLC)
    {
        for(i=0;i<symLC->nsyms;i++)
        {
            if(nlist[i].n_type & N_STAB)
                ;
            else if((nlist[i].n_type & N_TYPE) == N_SECT)
            {
                if(nlist[i].n_type & N_EXT)
                {
                    char *nm = image + symLC->stroff + nlist[i].n_un.n_strx;
                    if ((nlist[i].n_desc & N_WEAK_DEF) && lookupSymbol(nm)) {
                        // weak definition, and we already have a definition
                        IF_DEBUG(linker, debugBelch("    weak: %s\n", nm));
                    }
                    else
                    {
                            IF_DEBUG(linker, debugBelch("ocGetNames_MachO: inserting %s\n", nm));
                            ghciInsertSymbolTable(oc->fileName, symhash, nm,
                                                    image
                                                    + sections[nlist[i].n_sect-1].offset
                                                    - sections[nlist[i].n_sect-1].addr
                                                    + nlist[i].n_value,
                                                    HS_BOOL_FALSE,
                                                    oc);
                            oc->symbols[curSymbol++] = nm;
                    }
                }
                else
                {
                    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: \t...not external, skipping\n"));
                }
            }
            else
            {
                IF_DEBUG(linker, debugBelch("ocGetNames_MachO: \t...not defined in this section, skipping\n"));
            }
        }
    }

    commonStorage = stgCallocBytes(1,commonSize,"ocGetNames_MachO(common symbols)");
    commonCounter = (unsigned long)commonStorage;

    if (symLC) {
        for (i = 0; i < symLC->nsyms; i++) {
            if((nlist[i].n_type & N_TYPE) == N_UNDF
             && (nlist[i].n_type & N_EXT)
             && (nlist[i].n_value != 0)) {

                char *nm = image + symLC->stroff + nlist[i].n_un.n_strx;
                unsigned long sz = nlist[i].n_value;

                nlist[i].n_value = commonCounter;

                IF_DEBUG(linker, debugBelch("ocGetNames_MachO: inserting common symbol: %s\n", nm));
                ghciInsertSymbolTable(oc->fileName, symhash, nm,
                                       (void*)commonCounter, HS_BOOL_FALSE, oc);
                oc->symbols[curSymbol++] = nm;

                commonCounter += sz;
            }
        }
    }

    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: done\n"));
    return 1;
}

static int
ocResolve_MachO(ObjectCode* oc)
{
    char *image = (char*) oc->image;
    struct mach_header *header = (struct mach_header*) image;
    struct load_command *lc = (struct load_command*) (image + sizeof(struct mach_header));
    unsigned i;
    struct segment_command *segLC = NULL;
    struct section *sections;
    struct symtab_command *symLC = NULL;
    struct dysymtab_command *dsymLC = NULL;
    struct nlist *nlist;

    IF_DEBUG(linker, debugBelch("ocResolve_MachO: start\n"));
    for (i = 0; i < header->ncmds; i++)
    {
        if (lc->cmd == LC_SEGMENT || lc->cmd == LC_SEGMENT_64) {
            segLC = (struct segment_command*) lc;
            IF_DEBUG(linker, debugBelch("ocResolve_MachO: found a 32 or 64 bit segment load command\n"));
        }
        else if (lc->cmd == LC_SYMTAB) {
            symLC = (struct symtab_command*) lc;
            IF_DEBUG(linker, debugBelch("ocResolve_MachO: found a symbol table load command\n"));
        }
        else if (lc->cmd == LC_DYSYMTAB) {
            dsymLC = (struct dysymtab_command*) lc;
            IF_DEBUG(linker, debugBelch("ocResolve_MachO: found a dynamic symbol table load command\n"));
        }

        lc = (struct load_command *) ( ((char*)lc) + lc->cmdsize );
    }

    sections = (struct section*) (segLC+1);
    nlist = symLC ? (struct nlist*) (image + symLC->symoff)
                  : NULL;

    if(dsymLC)
    {
        unsigned long *indirectSyms
            = (unsigned long*) (image + dsymLC->indirectsymoff);

        IF_DEBUG(linker, debugBelch("ocResolve_MachO: resolving dsymLC\n"));
        for (i = 0; i < segLC->nsects; i++)
        {
            if(    !strcmp(sections[i].sectname,"__la_symbol_ptr")
                || !strcmp(sections[i].sectname,"__la_sym_ptr2")
                || !strcmp(sections[i].sectname,"__la_sym_ptr3"))
            {
                if(!resolveImports(oc,image,symLC,&sections[i],indirectSyms,nlist))
                    return 0;
            }
            else if(!strcmp(sections[i].sectname,"__nl_symbol_ptr")
                ||  !strcmp(sections[i].sectname,"__pointers"))
            {
                if(!resolveImports(oc,image,symLC,&sections[i],indirectSyms,nlist))
                    return 0;
            }
            else if(!strcmp(sections[i].sectname,"__jump_table"))
            {
                if(!resolveImports(oc,image,symLC,&sections[i],indirectSyms,nlist))
                    return 0;
            }
            else
            {
                IF_DEBUG(linker, debugBelch("ocResolve_MachO: unknown section\n"));
            }
        }
    }

    for(i=0;i<segLC->nsects;i++)
    {
            IF_DEBUG(linker, debugBelch("ocResolve_MachO: relocating section %d\n", i));

        if (!relocateSection(oc,image,symLC,nlist,segLC->nsects,sections,&sections[i]))
            return 0;
    }

#if defined (powerpc_HOST_ARCH)
    ocFlushInstructionCache( oc );
#endif

    return 1;
}

static int ocRunInit_MachO ( ObjectCode *oc )
{
    char *image = (char*) oc->image;
    struct mach_header *header = (struct mach_header*) image;
    struct load_command *lc = (struct load_command*) (image + sizeof(struct mach_header));
    struct segment_command *segLC = NULL;
    struct section *sections;
    nat i;

    for (i = 0; i < header->ncmds; i++) {
        if (lc->cmd == LC_SEGMENT || lc->cmd == LC_SEGMENT_64) {
            segLC = (struct segment_command*) lc;
        }
        lc = (struct load_command *) ( ((char*)lc) + lc->cmdsize );
    }
    if (!segLC) {
        barf("ocRunInit_MachO: no segment load command");
    }
    sections = (struct section*) (segLC+1);

    int argc, envc;
    char **argv, **envv;

    getProgArgv(&argc, &argv);
    getProgEnvv(&envc, &envv);

    for (i = 0; i < segLC->nsects; i++) {
        // ToDo: replace this with a proper check for the S_MOD_INIT_FUNC_POINTERS
        // flag.  We should do this elsewhere in the Mach-O linker code
        // too.  Note that the system linker will *refuse* to honor
        // sections which don't have this flag, so this could cause
        // weird behavior divergence (albeit reproduceable).
        if (0 == strcmp(sections[i].sectname,"__mod_init_func")) {
            char *init_startC = image + sections[i].offset;
            init_t *init = (init_t*)init_startC;
            init_t *init_end = (init_t*)(init_startC + sections[i].size);
            for (; init < init_end; init++) {
                (*init)(argc, argv, envv);
            }
        }
    }

    freeProgEnvv(envc, envv);
    return 1;
}

#ifdef powerpc_HOST_ARCH
/*
 * The Mach-O object format uses leading underscores. But not everywhere.
 * There is a small number of runtime support functions defined in
 * libcc_dynamic.a whose name does not have a leading underscore.
 * As a consequence, we can't get their address from C code.
 * We have to use inline assembler just to take the address of a function.
 * Yuck.
 */

extern void* symbolsWithoutUnderscore[];

static void
machoInitSymbolsWithoutUnderscore(void)
{
    void **p = symbolsWithoutUnderscore;
    __asm__ volatile(".globl _symbolsWithoutUnderscore\n.data\n_symbolsWithoutUnderscore:");

#undef SymI_NeedsProto
#define SymI_NeedsProto(x)  \
    __asm__ volatile(".long " # x);

    RTS_MACHO_NOUNDERLINE_SYMBOLS

    __asm__ volatile(".text");

#undef SymI_NeedsProto
#define SymI_NeedsProto(x)  \
    ghciInsertSymbolTable("(GHCi built-in symbols)", symhash, #x, *p++, HS_BOOL_FALSE, NULL);

    RTS_MACHO_NOUNDERLINE_SYMBOLS

#undef SymI_NeedsProto
}
#endif

#ifndef USE_MMAP
/*
 * Figure out by how much to shift the entire Mach-O file in memory
 * when loading so that its single segment ends up 16-byte-aligned
 */
static int
machoGetMisalignment( FILE * f )
{
    struct mach_header header;
    int misalignment;

    {
        int n = fread(&header, sizeof(header), 1, f);
        if (n != 1) {
            barf("machoGetMisalignment: can't read the Mach-O header");
        }
    }
    fseek(f, -sizeof(header), SEEK_CUR);

#if x86_64_HOST_ARCH || powerpc64_HOST_ARCH
    if(header.magic != MH_MAGIC_64) {
        barf("Bad magic. Expected: %08x, got: %08x.",
             MH_MAGIC_64, header.magic);
    }
#else
    if(header.magic != MH_MAGIC) {
        barf("Bad magic. Expected: %08x, got: %08x.",
             MH_MAGIC, header.magic);
    }
#endif

    misalignment = (header.sizeofcmds + sizeof(header))
                    & 0xF;

    return misalignment ? (16 - misalignment) : 0;
}
#endif

#endif
