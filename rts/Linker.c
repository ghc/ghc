/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2004
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
#ifdef __linux__
#define _GNU_SOURCE
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
#include "Stable.h"

#if !defined(mingw32_HOST_OS)
#include "posix/Signals.h"
#endif

#if defined(mingw32_HOST_OS)
// get protos for is*()
#include <ctype.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

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

#if defined(linux_HOST_OS) || defined(freebsd_HOST_OS) || defined(dragonfly_HOST_OS) || defined(netbsd_HOST_OS) || defined(openbsd_HOST_OS) || defined(darwin_HOST_OS)
#define USE_MMAP
#include <fcntl.h>
#include <sys/mman.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#endif

#if defined(linux_HOST_OS) || defined(solaris2_HOST_OS) || defined(freebsd_HOST_OS) || defined(dragonfly_HOST_OS) || defined(netbsd_HOST_OS) || defined(openbsd_HOST_OS)
#  define OBJFORMAT_ELF
#  include <regex.h>	// regex is already used by dlopen() so this is OK
			// to use here without requiring an additional lib
#elif defined(cygwin32_HOST_OS) || defined (mingw32_HOST_OS)
#  define OBJFORMAT_PEi386
#  include <windows.h>
#  include <math.h>
#elif defined(darwin_HOST_OS)
#  define OBJFORMAT_MACHO
#  include <regex.h>
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

/* Hash table mapping symbol names to Symbol */
static /*Str*/HashTable *symhash;

/* Hash table mapping symbol names to StgStablePtr */
static /*Str*/HashTable *stablehash;

/* List of currently loaded objects */
ObjectCode *objects = NULL;	/* initially empty */

#if defined(OBJFORMAT_ELF)
static int ocVerifyImage_ELF    ( ObjectCode* oc );
static int ocGetNames_ELF       ( ObjectCode* oc );
static int ocResolve_ELF        ( ObjectCode* oc );
#if defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
static int ocAllocateSymbolExtras_ELF ( ObjectCode* oc );
#endif
#elif defined(OBJFORMAT_PEi386)
static int ocVerifyImage_PEi386 ( ObjectCode* oc );
static int ocGetNames_PEi386    ( ObjectCode* oc );
static int ocResolve_PEi386     ( ObjectCode* oc );
static void *lookupSymbolInDLLs ( unsigned char *lbl );
static void zapTrailingAtSign   ( unsigned char *sym );
#elif defined(OBJFORMAT_MACHO)
static int ocVerifyImage_MachO    ( ObjectCode* oc );
static int ocGetNames_MachO       ( ObjectCode* oc );
static int ocResolve_MachO        ( ObjectCode* oc );

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
#if defined(x86_64_HOST_ARCH)

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

#define Maybe_Stable_Names      SymI_HasProto(stg_mkWeakzh)			\
      				SymI_HasProto(stg_mkWeakForeignEnvzh)		\
      				SymI_HasProto(stg_makeStableNamezh)		\
      				SymI_HasProto(stg_finalizzeWeakzh)

#if !defined (mingw32_HOST_OS)
#define RTS_POSIX_ONLY_SYMBOLS                  \
      SymI_HasProto(__hscore_get_saved_termios)	\
      SymI_HasProto(__hscore_set_saved_termios)	\
      SymI_HasProto(shutdownHaskellAndSignal)	\
      SymI_HasProto(lockFile)                   \
      SymI_HasProto(unlockFile)                 \
      SymI_HasProto(signal_handlers)		\
      SymI_HasProto(stg_sig_install)		\
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

#elif !defined(mingw32_HOST_OS)
#define RTS_MINGW_ONLY_SYMBOLS /**/
#define RTS_CYGWIN_ONLY_SYMBOLS /**/
#else /* defined(mingw32_HOST_OS) */
#define RTS_POSIX_ONLY_SYMBOLS  /**/
#define RTS_CYGWIN_ONLY_SYMBOLS /**/

/* Extra syms gen'ed by mingw-2's gcc-3.2: */
#if __GNUC__>=3
#define RTS_MINGW_EXTRA_SYMS                    \
      SymI_NeedsProto(_imp____mb_cur_max)       \
      SymI_NeedsProto(_imp___pctype)
#else
#define RTS_MINGW_EXTRA_SYMS
#endif

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

/* These are statically linked from the mingw libraries into the ghc
   executable, so we have to employ this hack. */
#define RTS_MINGW_ONLY_SYMBOLS                           \
      SymI_HasProto(stg_asyncReadzh)			 \
      SymI_HasProto(stg_asyncWritezh)			 \
      SymI_HasProto(stg_asyncDoProczh)			 \
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
      SymI_NeedsProto(_alloca)                           \
      SymI_HasProto(isxdigit)                          \
      SymI_HasProto(isupper)                           \
      SymI_HasProto(ispunct)                           \
      SymI_HasProto(islower)                           \
      SymI_HasProto(isspace)                           \
      SymI_HasProto(isprint)                           \
      SymI_HasProto(isdigit)                           \
      SymI_HasProto(iscntrl)                           \
      SymI_HasProto(isalpha)                           \
      SymI_HasProto(isalnum)                           \
      SymI_HasProto(isascii)                           \
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
      SymI_HasProto(erf)                                \
      SymI_HasProto(erfc)                                \
      SymI_HasProto(erff)                                \
      SymI_HasProto(erfcf)                               \
      SymI_HasProto(memcpy)                              \
      SymI_HasProto(rts_InstallConsoleEvent)             \
      SymI_HasProto(rts_ConsoleHandlerDone)              \
      SymI_NeedsProto(mktime)                            \
      SymI_NeedsProto(_imp___timezone)                   \
      SymI_NeedsProto(_imp___tzname)                     \
      SymI_NeedsProto(_imp__tzname)                      \
      SymI_NeedsProto(_imp___iob)                        \
      SymI_NeedsProto(_imp___osver)                      \
      SymI_NeedsProto(localtime)                         \
      SymI_NeedsProto(gmtime)                            \
      SymI_NeedsProto(opendir)                           \
      SymI_NeedsProto(readdir)                           \
      SymI_NeedsProto(rewinddir)                         \
      RTS_MINGW_EXTRA_SYMS                               \
      RTS_MINGW_GETTIMEOFDAY_SYM		         \
      SymI_NeedsProto(closedir)
#endif

#if defined(darwin_HOST_OS) && HAVE_PRINTF_LDBLSTUB
#define RTS_DARWIN_ONLY_SYMBOLS			            \
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
#define RTS_USER_SIGNALS_SYMBOLS \
   SymI_HasProto(setIOManagerPipe) \
   SymI_HasProto(ioManagerWakeup) \
   SymI_HasProto(ioManagerSync) \
   SymI_HasProto(blockUserSignals) \
   SymI_HasProto(unblockUserSignals)
#else
#define RTS_USER_SIGNALS_SYMBOLS     \
   SymI_HasProto(ioManagerWakeup) \
   SymI_HasProto(sendIOManagerEvent) \
   SymI_HasProto(readIOManagerEvent) \
   SymI_HasProto(getIOManagerEvent)  \
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
#define RTS_RET_SYMBOLS 		                \
      SymI_HasProto(stg_enter_ret)			\
      SymI_HasProto(stg_gc_fun_ret)			\
      SymI_HasProto(stg_ap_v_ret)			\
      SymI_HasProto(stg_ap_f_ret)			\
      SymI_HasProto(stg_ap_d_ret)			\
      SymI_HasProto(stg_ap_l_ret)			\
      SymI_HasProto(stg_ap_n_ret)			\
      SymI_HasProto(stg_ap_p_ret)			\
      SymI_HasProto(stg_ap_pv_ret)			\
      SymI_HasProto(stg_ap_pp_ret)			\
      SymI_HasProto(stg_ap_ppv_ret)			\
      SymI_HasProto(stg_ap_ppp_ret)			\
      SymI_HasProto(stg_ap_pppv_ret)			\
      SymI_HasProto(stg_ap_pppp_ret)			\
      SymI_HasProto(stg_ap_ppppp_ret)			\
      SymI_HasProto(stg_ap_pppppp_ret)
#endif

/* Modules compiled with -ticky may mention ticky counters */
/* This list should marry up with the one in $(TOP)/includes/stg/Ticky.h */
#define RTS_TICKY_SYMBOLS                       \
      SymI_NeedsProto(ticky_entry_ctrs)         \
      SymI_NeedsProto(top_ct)                   \
                                                \
      SymI_HasProto(ENT_VIA_NODE_ctr)		\
      SymI_HasProto(ENT_STATIC_THK_ctr)		\
      SymI_HasProto(ENT_DYN_THK_ctr)		\
      SymI_HasProto(ENT_STATIC_FUN_DIRECT_ctr)	\
      SymI_HasProto(ENT_DYN_FUN_DIRECT_ctr)	\
      SymI_HasProto(ENT_STATIC_CON_ctr)		\
      SymI_HasProto(ENT_DYN_CON_ctr)		\
      SymI_HasProto(ENT_STATIC_IND_ctr)		\
      SymI_HasProto(ENT_DYN_IND_ctr)		\
      SymI_HasProto(ENT_PERM_IND_ctr)		\
      SymI_HasProto(ENT_PAP_ctr)		\
      SymI_HasProto(ENT_AP_ctr)		        \
      SymI_HasProto(ENT_AP_STACK_ctr)		\
      SymI_HasProto(ENT_BH_ctr)	        	\
      SymI_HasProto(UNKNOWN_CALL_ctr)		\
      SymI_HasProto(SLOW_CALL_v_ctr)		\
      SymI_HasProto(SLOW_CALL_f_ctr)		\
      SymI_HasProto(SLOW_CALL_d_ctr)		\
      SymI_HasProto(SLOW_CALL_l_ctr)		\
      SymI_HasProto(SLOW_CALL_n_ctr)		\
      SymI_HasProto(SLOW_CALL_p_ctr)		\
      SymI_HasProto(SLOW_CALL_pv_ctr)		\
      SymI_HasProto(SLOW_CALL_pp_ctr)		\
      SymI_HasProto(SLOW_CALL_ppv_ctr)		\
      SymI_HasProto(SLOW_CALL_ppp_ctr)		\
      SymI_HasProto(SLOW_CALL_pppv_ctr)		\
      SymI_HasProto(SLOW_CALL_pppp_ctr)		\
      SymI_HasProto(SLOW_CALL_ppppp_ctr)		\
      SymI_HasProto(SLOW_CALL_pppppp_ctr)		\
      SymI_HasProto(SLOW_CALL_OTHER_ctr)		\
      SymI_HasProto(ticky_slow_call_unevald)            \
      SymI_HasProto(SLOW_CALL_ctr)		        \
      SymI_HasProto(MULTI_CHUNK_SLOW_CALL_ctr)		\
      SymI_HasProto(MULTI_CHUNK_SLOW_CALL_CHUNKS_ctr)	\
      SymI_HasProto(KNOWN_CALL_ctr)		        \
      SymI_HasProto(KNOWN_CALL_TOO_FEW_ARGS_ctr)	\
      SymI_HasProto(KNOWN_CALL_EXTRA_ARGS_ctr)		\
      SymI_HasProto(SLOW_CALL_FUN_TOO_FEW_ctr)		\
      SymI_HasProto(SLOW_CALL_FUN_CORRECT_ctr)		\
      SymI_HasProto(SLOW_CALL_FUN_TOO_MANY_ctr)		\
      SymI_HasProto(SLOW_CALL_PAP_TOO_FEW_ctr)		\
      SymI_HasProto(SLOW_CALL_PAP_CORRECT_ctr)		\
      SymI_HasProto(SLOW_CALL_PAP_TOO_MANY_ctr)		\
      SymI_HasProto(SLOW_CALL_UNEVALD_ctr)		\
      SymI_HasProto(UPDF_OMITTED_ctr)		\
      SymI_HasProto(UPDF_PUSHED_ctr)		\
      SymI_HasProto(CATCHF_PUSHED_ctr)		\
      SymI_HasProto(UPDF_RCC_PUSHED_ctr)	\
      SymI_HasProto(UPDF_RCC_OMITTED_ctr)	\
      SymI_HasProto(UPD_SQUEEZED_ctr)		\
      SymI_HasProto(UPD_CON_IN_NEW_ctr)		\
      SymI_HasProto(UPD_CON_IN_PLACE_ctr)	\
      SymI_HasProto(UPD_PAP_IN_NEW_ctr)		\
      SymI_HasProto(UPD_PAP_IN_PLACE_ctr)	\
      SymI_HasProto(ALLOC_HEAP_ctr)		\
      SymI_HasProto(ALLOC_HEAP_tot)             \
      SymI_HasProto(ALLOC_FUN_ctr)		\
      SymI_HasProto(ALLOC_FUN_adm)              \
      SymI_HasProto(ALLOC_FUN_gds)              \
      SymI_HasProto(ALLOC_FUN_slp)              \
      SymI_HasProto(UPD_NEW_IND_ctr)		\
      SymI_HasProto(UPD_NEW_PERM_IND_ctr)	\
      SymI_HasProto(UPD_OLD_IND_ctr)		\
      SymI_HasProto(UPD_OLD_PERM_IND_ctr)		\
      SymI_HasProto(UPD_BH_UPDATABLE_ctr)		\
      SymI_HasProto(UPD_BH_SINGLE_ENTRY_ctr)		\
      SymI_HasProto(UPD_CAF_BH_UPDATABLE_ctr)		\
      SymI_HasProto(UPD_CAF_BH_SINGLE_ENTRY_ctr)	\
      SymI_HasProto(GC_SEL_ABANDONED_ctr)		\
      SymI_HasProto(GC_SEL_MINOR_ctr)		\
      SymI_HasProto(GC_SEL_MAJOR_ctr)		\
      SymI_HasProto(GC_FAILED_PROMOTION_ctr)	\
      SymI_HasProto(ALLOC_UP_THK_ctr)		\
      SymI_HasProto(ALLOC_SE_THK_ctr)		\
      SymI_HasProto(ALLOC_THK_adm)		\
      SymI_HasProto(ALLOC_THK_gds)		\
      SymI_HasProto(ALLOC_THK_slp)		\
      SymI_HasProto(ALLOC_CON_ctr)		\
      SymI_HasProto(ALLOC_CON_adm)		\
      SymI_HasProto(ALLOC_CON_gds)		\
      SymI_HasProto(ALLOC_CON_slp)		\
      SymI_HasProto(ALLOC_TUP_ctr)		\
      SymI_HasProto(ALLOC_TUP_adm)		\
      SymI_HasProto(ALLOC_TUP_gds)		\
      SymI_HasProto(ALLOC_TUP_slp)		\
      SymI_HasProto(ALLOC_BH_ctr)		\
      SymI_HasProto(ALLOC_BH_adm)		\
      SymI_HasProto(ALLOC_BH_gds)		\
      SymI_HasProto(ALLOC_BH_slp)		\
      SymI_HasProto(ALLOC_PRIM_ctr)		\
      SymI_HasProto(ALLOC_PRIM_adm)		\
      SymI_HasProto(ALLOC_PRIM_gds)		\
      SymI_HasProto(ALLOC_PRIM_slp)		\
      SymI_HasProto(ALLOC_PAP_ctr)		\
      SymI_HasProto(ALLOC_PAP_adm)		\
      SymI_HasProto(ALLOC_PAP_gds)		\
      SymI_HasProto(ALLOC_PAP_slp)		\
      SymI_HasProto(ALLOC_TSO_ctr)		\
      SymI_HasProto(ALLOC_TSO_adm)		\
      SymI_HasProto(ALLOC_TSO_gds)		\
      SymI_HasProto(ALLOC_TSO_slp)		\
      SymI_HasProto(RET_NEW_ctr)		\
      SymI_HasProto(RET_OLD_ctr)		\
      SymI_HasProto(RET_UNBOXED_TUP_ctr)	\
      SymI_HasProto(RET_SEMI_loads_avoided)


// On most platforms, the garbage collector rewrites references
//	to small integer and char objects to a set of common, shared ones.
//
// We don't do this when compiling to Windows DLLs at the moment because
//	it doesn't support cross package data references well.
//
#if defined(__PIC__) && defined(mingw32_HOST_OS)
#define RTS_INTCHAR_SYMBOLS
#else
#define RTS_INTCHAR_SYMBOLS				\
      SymI_HasProto(stg_CHARLIKE_closure)		\
      SymI_HasProto(stg_INTLIKE_closure)		
#endif


#define RTS_SYMBOLS				        \
      Maybe_Stable_Names			        \
      RTS_TICKY_SYMBOLS                                 \
      SymI_HasProto(StgReturn)				\
      SymI_HasProto(stg_enter_info)			\
      SymI_HasProto(stg_gc_void_info)			\
      SymI_HasProto(__stg_gc_enter_1)			\
      SymI_HasProto(stg_gc_noregs)			\
      SymI_HasProto(stg_gc_unpt_r1_info)		\
      SymI_HasProto(stg_gc_unpt_r1)			\
      SymI_HasProto(stg_gc_unbx_r1_info)		\
      SymI_HasProto(stg_gc_unbx_r1)			\
      SymI_HasProto(stg_gc_f1_info)			\
      SymI_HasProto(stg_gc_f1)				\
      SymI_HasProto(stg_gc_d1_info)			\
      SymI_HasProto(stg_gc_d1)				\
      SymI_HasProto(stg_gc_l1_info)			\
      SymI_HasProto(stg_gc_l1)				\
      SymI_HasProto(__stg_gc_fun)			\
      SymI_HasProto(stg_gc_fun_info)			\
      SymI_HasProto(stg_gc_gen)				\
      SymI_HasProto(stg_gc_gen_info)			\
      SymI_HasProto(stg_gc_gen_hp)			\
      SymI_HasProto(stg_gc_ut)				\
      SymI_HasProto(stg_gen_yield)			\
      SymI_HasProto(stg_yield_noregs)			\
      SymI_HasProto(stg_yield_to_interpreter)		\
      SymI_HasProto(stg_gen_block)			\
      SymI_HasProto(stg_block_noregs)			\
      SymI_HasProto(stg_block_1)			\
      SymI_HasProto(stg_block_takemvar)			\
      SymI_HasProto(stg_block_putmvar)			\
      MAIN_CAP_SYM                                      \
      SymI_HasProto(MallocFailHook)			\
      SymI_HasProto(OnExitHook)				\
      SymI_HasProto(OutOfHeapHook)			\
      SymI_HasProto(StackOverflowHook)			\
      SymI_HasProto(addDLL)               		\
      SymI_HasProto(__int_encodeDouble)			\
      SymI_HasProto(__word_encodeDouble)		\
      SymI_HasProto(__2Int_encodeDouble)		\
      SymI_HasProto(__int_encodeFloat)			\
      SymI_HasProto(__word_encodeFloat)			\
      SymI_HasProto(stg_atomicallyzh)			\
      SymI_HasProto(barf)				\
      SymI_HasProto(debugBelch)				\
      SymI_HasProto(errorBelch)				\
      SymI_HasProto(sysErrorBelch)                      \
      SymI_HasProto(stg_getMaskingStatezh)		\
      SymI_HasProto(stg_maskAsyncExceptionszh)	        \
      SymI_HasProto(stg_maskUninterruptiblezh)	        \
      SymI_HasProto(stg_catchzh)			\
      SymI_HasProto(stg_catchRetryzh)			\
      SymI_HasProto(stg_catchSTMzh)			\
      SymI_HasProto(stg_checkzh)                        \
      SymI_HasProto(closure_flags)                      \
      SymI_HasProto(cmp_thread)				\
      SymI_HasProto(createAdjustor)			\
      SymI_HasProto(stg_decodeDoublezu2Intzh)		\
      SymI_HasProto(stg_decodeFloatzuIntzh)		\
      SymI_HasProto(defaultsHook)			\
      SymI_HasProto(stg_delayzh)			\
      SymI_HasProto(stg_deRefWeakzh)			\
      SymI_HasProto(stg_deRefStablePtrzh)		\
      SymI_HasProto(dirty_MUT_VAR)			\
      SymI_HasProto(stg_forkzh)			        \
      SymI_HasProto(stg_forkOnzh)			\
      SymI_HasProto(forkProcess)			\
      SymI_HasProto(forkOS_createThread)		\
      SymI_HasProto(freeHaskellFunctionPtr)		\
      SymI_HasProto(getOrSetTypeableStore)		\
      SymI_HasProto(getOrSetGHCConcSignalHandlerStore)		\
      SymI_HasProto(getOrSetGHCConcPendingEventsStore)		\
      SymI_HasProto(getOrSetGHCConcPendingDelaysStore)		\
      SymI_HasProto(getOrSetGHCConcIOManagerThreadStore)	\
      SymI_HasProto(getOrSetGHCConcProddingStore)		\
      SymI_HasProto(genSymZh)				\
      SymI_HasProto(genericRaise)			\
      SymI_HasProto(getProgArgv)			\
      SymI_HasProto(getFullProgArgv)			\
      SymI_HasProto(getStablePtr)			\
      SymI_HasProto(hs_init)				\
      SymI_HasProto(hs_exit)				\
      SymI_HasProto(hs_set_argv)			\
      SymI_HasProto(hs_add_root)			\
      SymI_HasProto(hs_perform_gc)			\
      SymI_HasProto(hs_free_stable_ptr)			\
      SymI_HasProto(hs_free_fun_ptr)			\
      SymI_HasProto(hs_hpc_rootModule)		        \
      SymI_HasProto(hs_hpc_module)		        \
      SymI_HasProto(initLinker)				\
      SymI_HasProto(stg_unpackClosurezh)                \
      SymI_HasProto(stg_getApStackValzh)                \
      SymI_HasProto(stg_getSparkzh)                     \
      SymI_HasProto(stg_numSparkszh)                    \
      SymI_HasProto(stg_isCurrentThreadBoundzh)	        \
      SymI_HasProto(stg_isEmptyMVarzh)			\
      SymI_HasProto(stg_killThreadzh)			\
      SymI_HasProto(loadObj)          			\
      SymI_HasProto(insertStableSymbol) 		\
      SymI_HasProto(insertSymbol)     			\
      SymI_HasProto(lookupSymbol)     			\
      SymI_HasProto(stg_makeStablePtrzh)		\
      SymI_HasProto(stg_mkApUpd0zh)			\
      SymI_HasProto(stg_myThreadIdzh)			\
      SymI_HasProto(stg_labelThreadzh)                  \
      SymI_HasProto(stg_newArrayzh)			\
      SymI_HasProto(stg_newBCOzh)			\
      SymI_HasProto(stg_newByteArrayzh)  		\
      SymI_HasProto_redirect(newCAF, newDynCAF)		\
      SymI_HasProto(stg_newMVarzh)			\
      SymI_HasProto(stg_newMutVarzh)			\
      SymI_HasProto(stg_newTVarzh)			\
      SymI_HasProto(stg_noDuplicatezh)			\
      SymI_HasProto(stg_atomicModifyMutVarzh)		\
      SymI_HasProto(stg_newPinnedByteArrayzh)		\
      SymI_HasProto(stg_newAlignedPinnedByteArrayzh)	\
      SymI_HasProto(newSpark)				\
      SymI_HasProto(performGC)				\
      SymI_HasProto(performMajorGC)			\
      SymI_HasProto(prog_argc)				\
      SymI_HasProto(prog_argv)				\
      SymI_HasProto(stg_putMVarzh)			\
      SymI_HasProto(stg_raisezh)			\
      SymI_HasProto(stg_raiseIOzh)			\
      SymI_HasProto(stg_readTVarzh)			\
      SymI_HasProto(stg_readTVarIOzh)			\
      SymI_HasProto(resumeThread)			\
      SymI_HasProto(resolveObjs)                        \
      SymI_HasProto(stg_retryzh)                        \
      SymI_HasProto(rts_apply)				\
      SymI_HasProto(rts_checkSchedStatus)		\
      SymI_HasProto(rts_eval)				\
      SymI_HasProto(rts_evalIO)				\
      SymI_HasProto(rts_evalLazyIO)			\
      SymI_HasProto(rts_evalStableIO)			\
      SymI_HasProto(rts_eval_)				\
      SymI_HasProto(rts_getBool)			\
      SymI_HasProto(rts_getChar)			\
      SymI_HasProto(rts_getDouble)			\
      SymI_HasProto(rts_getFloat)			\
      SymI_HasProto(rts_getInt)				\
      SymI_HasProto(rts_getInt8)			\
      SymI_HasProto(rts_getInt16)			\
      SymI_HasProto(rts_getInt32)			\
      SymI_HasProto(rts_getInt64)			\
      SymI_HasProto(rts_getPtr)				\
      SymI_HasProto(rts_getFunPtr)			\
      SymI_HasProto(rts_getStablePtr)			\
      SymI_HasProto(rts_getThreadId)			\
      SymI_HasProto(rts_getWord)			\
      SymI_HasProto(rts_getWord8)			\
      SymI_HasProto(rts_getWord16)			\
      SymI_HasProto(rts_getWord32)			\
      SymI_HasProto(rts_getWord64)			\
      SymI_HasProto(rts_lock)				\
      SymI_HasProto(rts_mkBool)				\
      SymI_HasProto(rts_mkChar)				\
      SymI_HasProto(rts_mkDouble)			\
      SymI_HasProto(rts_mkFloat)			\
      SymI_HasProto(rts_mkInt)				\
      SymI_HasProto(rts_mkInt8)				\
      SymI_HasProto(rts_mkInt16)			\
      SymI_HasProto(rts_mkInt32)			\
      SymI_HasProto(rts_mkInt64)			\
      SymI_HasProto(rts_mkPtr)				\
      SymI_HasProto(rts_mkFunPtr)			\
      SymI_HasProto(rts_mkStablePtr)			\
      SymI_HasProto(rts_mkString)			\
      SymI_HasProto(rts_mkWord)				\
      SymI_HasProto(rts_mkWord8)			\
      SymI_HasProto(rts_mkWord16)			\
      SymI_HasProto(rts_mkWord32)			\
      SymI_HasProto(rts_mkWord64)			\
      SymI_HasProto(rts_unlock)				\
      SymI_HasProto(rts_unsafeGetMyCapability)          \
      SymI_HasProto(rtsSupportsBoundThreads)		\
      SymI_HasProto(setProgArgv)			\
      SymI_HasProto(startupHaskell)			\
      SymI_HasProto(shutdownHaskell)			\
      SymI_HasProto(shutdownHaskellAndExit)		\
      SymI_HasProto(stable_ptr_table)			\
      SymI_HasProto(stackOverflow)			\
      SymI_HasProto(stg_CAF_BLACKHOLE_info)		\
      SymI_HasProto(stg_BLACKHOLE_info)			\
      SymI_HasProto(__stg_EAGER_BLACKHOLE_info)		\
      SymI_HasProto(stg_BLOCKING_QUEUE_CLEAN_info)      \
      SymI_HasProto(stg_BLOCKING_QUEUE_DIRTY_info)	\
      SymI_HasProto(startTimer)                         \
      SymI_HasProto(stg_MVAR_CLEAN_info)		\
      SymI_HasProto(stg_MVAR_DIRTY_info)		\
      SymI_HasProto(stg_IND_STATIC_info)		\
      SymI_HasProto(stg_ARR_WORDS_info)                 \
      SymI_HasProto(stg_MUT_ARR_PTRS_DIRTY_info)	\
      SymI_HasProto(stg_MUT_ARR_PTRS_FROZEN_info)	\
      SymI_HasProto(stg_MUT_ARR_PTRS_FROZEN0_info)	\
      SymI_HasProto(stg_WEAK_info)                      \
      SymI_HasProto(stg_ap_v_info)			\
      SymI_HasProto(stg_ap_f_info)			\
      SymI_HasProto(stg_ap_d_info)			\
      SymI_HasProto(stg_ap_l_info)			\
      SymI_HasProto(stg_ap_n_info)			\
      SymI_HasProto(stg_ap_p_info)			\
      SymI_HasProto(stg_ap_pv_info)			\
      SymI_HasProto(stg_ap_pp_info)			\
      SymI_HasProto(stg_ap_ppv_info)			\
      SymI_HasProto(stg_ap_ppp_info)			\
      SymI_HasProto(stg_ap_pppv_info)			\
      SymI_HasProto(stg_ap_pppp_info)			\
      SymI_HasProto(stg_ap_ppppp_info)			\
      SymI_HasProto(stg_ap_pppppp_info)			\
      SymI_HasProto(stg_ap_0_fast)			\
      SymI_HasProto(stg_ap_v_fast)			\
      SymI_HasProto(stg_ap_f_fast)			\
      SymI_HasProto(stg_ap_d_fast)			\
      SymI_HasProto(stg_ap_l_fast)			\
      SymI_HasProto(stg_ap_n_fast)			\
      SymI_HasProto(stg_ap_p_fast)			\
      SymI_HasProto(stg_ap_pv_fast)			\
      SymI_HasProto(stg_ap_pp_fast)			\
      SymI_HasProto(stg_ap_ppv_fast)			\
      SymI_HasProto(stg_ap_ppp_fast)			\
      SymI_HasProto(stg_ap_pppv_fast)			\
      SymI_HasProto(stg_ap_pppp_fast)			\
      SymI_HasProto(stg_ap_ppppp_fast)			\
      SymI_HasProto(stg_ap_pppppp_fast)			\
      SymI_HasProto(stg_ap_1_upd_info)			\
      SymI_HasProto(stg_ap_2_upd_info)			\
      SymI_HasProto(stg_ap_3_upd_info)			\
      SymI_HasProto(stg_ap_4_upd_info)			\
      SymI_HasProto(stg_ap_5_upd_info)			\
      SymI_HasProto(stg_ap_6_upd_info)			\
      SymI_HasProto(stg_ap_7_upd_info)			\
      SymI_HasProto(stg_exit)				\
      SymI_HasProto(stg_sel_0_upd_info)			\
      SymI_HasProto(stg_sel_10_upd_info)		\
      SymI_HasProto(stg_sel_11_upd_info)		\
      SymI_HasProto(stg_sel_12_upd_info)		\
      SymI_HasProto(stg_sel_13_upd_info)		\
      SymI_HasProto(stg_sel_14_upd_info)		\
      SymI_HasProto(stg_sel_15_upd_info)		\
      SymI_HasProto(stg_sel_1_upd_info)			\
      SymI_HasProto(stg_sel_2_upd_info)			\
      SymI_HasProto(stg_sel_3_upd_info)			\
      SymI_HasProto(stg_sel_4_upd_info)			\
      SymI_HasProto(stg_sel_5_upd_info)			\
      SymI_HasProto(stg_sel_6_upd_info)			\
      SymI_HasProto(stg_sel_7_upd_info)			\
      SymI_HasProto(stg_sel_8_upd_info)			\
      SymI_HasProto(stg_sel_9_upd_info)			\
      SymI_HasProto(stg_upd_frame_info)			\
      SymI_HasProto(stg_bh_upd_frame_info)		\
      SymI_HasProto(suspendThread)			\
      SymI_HasProto(stg_takeMVarzh)			\
      SymI_HasProto(stg_threadStatuszh)		        \
      SymI_HasProto(stg_tryPutMVarzh)			\
      SymI_HasProto(stg_tryTakeMVarzh)			\
      SymI_HasProto(stg_unmaskAsyncExceptionszh)	\
      SymI_HasProto(unloadObj)                          \
      SymI_HasProto(stg_unsafeThawArrayzh)		\
      SymI_HasProto(stg_waitReadzh)			\
      SymI_HasProto(stg_waitWritezh)			\
      SymI_HasProto(stg_writeTVarzh)			\
      SymI_HasProto(stg_yieldzh)                        \
      SymI_NeedsProto(stg_interp_constr_entry)          \
      SymI_HasProto(alloc_blocks_lim)                   \
      SymI_HasProto(g0)                                 \
      SymI_HasProto(allocate)                           \
      SymI_HasProto(allocateExec)	                \
      SymI_HasProto(freeExec)		                \
      SymI_HasProto(getAllocations)                     \
      SymI_HasProto(revertCAFs)                         \
      SymI_HasProto(RtsFlags)                           \
      SymI_NeedsProto(rts_breakpoint_io_action)		\
      SymI_NeedsProto(rts_stop_next_breakpoint)		\
      SymI_NeedsProto(rts_stop_on_exception)		\
      SymI_HasProto(stopTimer)				\
      SymI_HasProto(n_capabilities)			\
      SymI_HasProto(stg_traceCcszh)                     \
      SymI_HasProto(stg_traceEventzh)                   \
      RTS_USER_SIGNALS_SYMBOLS				\
      RTS_INTCHAR_SYMBOLS


// 64-bit support functions in libgcc.a
#if defined(__GNUC__) && SIZEOF_VOID_P <= 4
#define RTS_LIBGCC_SYMBOLS			       \
      SymI_NeedsProto(__divdi3)                        \
      SymI_NeedsProto(__udivdi3)                       \
      SymI_NeedsProto(__moddi3)                        \
      SymI_NeedsProto(__umoddi3)		       \
      SymI_NeedsProto(__muldi3)			       \
      SymI_NeedsProto(__ashldi3)		       \
      SymI_NeedsProto(__ashrdi3)		       \
      SymI_NeedsProto(__lshrdi3)
#else
#define RTS_LIBGCC_SYMBOLS
#endif

#if defined(darwin_HOST_OS) && defined(powerpc_HOST_ARCH)
      // Symbols that don't have a leading underscore
      // on Mac OS X. They have to receive special treatment,
      // see machoInitSymbolsWithoutUnderscore()
#define RTS_MACHO_NOUNDERLINE_SYMBOLS		\
      SymI_NeedsProto(saveFP)				\
      SymI_NeedsProto(restFP)
#endif

/* entirely bogus claims about types of these symbols */
#define SymI_NeedsProto(vvv)  extern void vvv(void);
#if defined(__PIC__) && defined(mingw32_HOST_OS)
#define SymE_HasProto(vvv)    SymE_HasProto(vvv);
#define SymE_NeedsProto(vvv)    extern void _imp__ ## vvv (void);
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
#define SymI_HasProto_redirect(vvv,xxx) \
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

static void ghciInsertStrHashTable ( char* obj_name,
                                     HashTable *table,
                                     char* key,
                                     void *data
				   )
{
   if (lookupHashTable(table, (StgWord)key) == NULL)
   {
      insertStrHashTable(table, (StgWord)key, data);
      return;
   }
   debugBelch(
      "\n\n"
      "GHCi runtime linker: fatal error: I found a duplicate definition for symbol\n"
      "   %s\n"
      "whilst processing object file\n"
      "   %s\n"
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
   exit(1);
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

    /* Make initLinker idempotent, so we can call it
       before evey relevant operation; that means we
       don't need to initialise the linker separately */
    if (linker_init_done == 1) { return; } else {
      linker_init_done = 1;
    }

#if defined(THREADED_RTS) && (defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO))
    initMutex(&dl_mutex);
#endif
    stablehash = allocStrHashTable();
    symhash = allocStrHashTable();

    /* populate the symbol table with stuff from the RTS */
    for (sym = rtsSyms; sym->lbl != NULL; sym++) {
	ghciInsertStrHashTable("(GHCi built-in symbols)",
                               symhash, sym->lbl, sym->addr);
    }
#   if defined(OBJFORMAT_MACHO) && defined(powerpc_HOST_ARCH)
    machoInitSymbolsWithoutUnderscore();
#   endif

#   if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
#   if defined(RTLD_DEFAULT)
    dl_prog_handle = RTLD_DEFAULT;
#   else
    dl_prog_handle = dlopen(NULL, RTLD_LAZY);
#   endif /* RTLD_DEFAULT */

    compileResult = regcomp(&re_invalid,
           "(([^ \t()])+\\.so([^ \t:()])*):([ \t])*invalid ELF header",
           REG_EXTENDED);
    ASSERT( compileResult == 0 );
    compileResult = regcomp(&re_realso,
           "GROUP *\\( *(([^ )])+)",
           REG_EXTENDED);
    ASSERT( compileResult == 0 );
#   endif

#if defined(x86_64_HOST_ARCH)
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
    addDLL("msvcrt");
    addDLL("kernel32");
#endif
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
      char*              name;
      struct _OpenedDLL* next;
      HINSTANCE instance;
   }
   OpenedDLL;

/* A list thereof. */
static OpenedDLL* opened_dlls = NULL;
#endif

#  if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)

static const char *
internal_dlopen(const char *dll_name)
{
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
   RELEASE_LOCK(&dl_mutex);
   //--------------- End critical section -------------------

   return errmsg;
}
#  endif

const char *
addDLL( char *dll_name )
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
	 return errmsg;	// return original error if open fails
      }
      // try to find a GROUP ( ... ) command
      while (fgets(line, MAXLINE, fp) != NULL) {
	 IF_DEBUG(linker, debugBelch("input line = %s", line));
	 if (regexec(&re_realso, line, (size_t) NMATCH, match, 0) == 0) {
            // success -- try to dlopen the first named file
            IF_DEBUG(linker, debugBelch("match%s\n",""));
	    line[match[1].rm_eo] = '\0';
	    errmsg = internal_dlopen(line+match[1].rm_so);
	    break;
	 }
	 // if control reaches here, no GROUP ( ... ) directive was found
	 // and the original error message is returned to the caller
      }
      fclose(fp);
   }
   return errmsg;

#  elif defined(OBJFORMAT_PEi386)
   /* ------------------- Win32 DLL loader ------------------- */

   char*      buf;
   OpenedDLL* o_dll;
   HINSTANCE  instance;

   initLinker();

   /* debugBelch("\naddDLL; dll_name = `%s'\n", dll_name); */

   /* See if we've already got it, and ignore if so. */
   for (o_dll = opened_dlls; o_dll != NULL; o_dll = o_dll->next) {
      if (0 == strcmp(o_dll->name, dll_name))
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

   buf = stgMallocBytes(strlen(dll_name) + 10, "addDLL");
   sprintf(buf, "%s.DLL", dll_name);
   instance = LoadLibrary(buf);
   if (instance == NULL) {
       if (GetLastError() != ERROR_MOD_NOT_FOUND) goto error;
       // KAA: allow loading of drivers (like winspool.drv)
       sprintf(buf, "%s.DRV", dll_name);
       instance = LoadLibrary(buf);
       if (instance == NULL) {
           if (GetLastError() != ERROR_MOD_NOT_FOUND) goto error;
           // #1883: allow loading of unix-style libfoo.dll DLLs
           sprintf(buf, "lib%s.DLL", dll_name);
           instance = LoadLibrary(buf);
           if (instance == NULL) {
               goto error;
           }
       }
   }
   stgFree(buf);

   /* Add this DLL to the list of DLLs in which to search for symbols. */
   o_dll = stgMallocBytes( sizeof(OpenedDLL), "addDLL" );
   o_dll->name     = stgMallocBytes(1+strlen(dll_name), "addDLL");
   strcpy(o_dll->name, dll_name);
   o_dll->instance = instance;
   o_dll->next     = opened_dlls;
   opened_dlls     = o_dll;

   return NULL;

error:
   stgFree(buf);
   sysErrorBelch(dll_name);

   /* LoadLibrary failed; return a ptr to the error msg. */
   return "addDLL: could not load DLL";

#  else
   barf("addDLL: not implemented on this platform");
#  endif
}

/* -----------------------------------------------------------------------------
 * insert a stable symbol in the hash table
 */

void
insertStableSymbol(char* obj_name, char* key, StgPtr p)
{
  ghciInsertStrHashTable(obj_name, stablehash, key, getStablePtr(p));
}


/* -----------------------------------------------------------------------------
 * insert a symbol in the hash table
 */
void
insertSymbol(char* obj_name, char* key, void* data)
{
  ghciInsertStrHashTable(obj_name, symhash, key, data);
}

/* -----------------------------------------------------------------------------
 * lookup a symbol in the hash table
 */
void *
lookupSymbol( char *lbl )
{
    void *val;
    initLinker() ;
    ASSERT(symhash != NULL);
    val = lookupStrHashTable(symhash, lbl);

    if (val == NULL) {
#       if defined(OBJFORMAT_ELF)
	return dlsym(dl_prog_handle, lbl);
#       elif defined(OBJFORMAT_MACHO)
#       if HAVE_DLFCN_H
        /* On OS X 10.3 and later, we use dlsym instead of the old legacy
           interface.

           HACK: On OS X, global symbols are prefixed with an underscore.
                 However, dlsym wants us to omit the leading underscore from the
                 symbol name. For now, we simply strip it off here (and ONLY
                 here).
        */
        ASSERT(lbl[0] == '_');
        return dlsym(dl_prog_handle, lbl+1);
#       else
	if(NSIsSymbolNameDefined(lbl)) {
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
	return val;
    }
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
            a = lookupStrHashTable(symhash, sym);
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

   pagesize = getpagesize();
   size = ROUND_UP(bytes, pagesize);

#if defined(x86_64_HOST_ARCH)
mmap_again:

   if (mmap_32bit_base != 0) {
       map_addr = mmap_32bit_base;
   }
#endif

   result = mmap(map_addr, size, PROT_EXEC|PROT_READ|PROT_WRITE,
		    MAP_PRIVATE|TRY_MAP_32BIT|fixed|flags, fd, 0);

   if (result == MAP_FAILED) {
       sysErrorBelch("mmap %lu bytes at %p",(lnat)size,map_addr);
       errorBelch("Try specifying an address with +RTS -xm<addr> -RTS");
       stg_exit(EXIT_FAILURE);
   }
   
#if defined(x86_64_HOST_ARCH)
   if (mmap_32bit_base != 0) {
       if (result == map_addr) {
           mmap_32bit_base = (StgWord8*)map_addr + size;
       } else {
           if ((W_)result > 0x80000000) {
               // oops, we were given memory over 2Gb
#if defined(freebsd_HOST_OS) || defined(dragonfly_HOST_OS)
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

   return result;
}
#endif // USE_MMAP

/* -----------------------------------------------------------------------------
 * Load an obj (populate the global symbol table, but don't resolve yet)
 *
 * Returns: 1 if ok, 0 on error.
 */
HsInt
loadObj( char *path )
{
   ObjectCode* oc;
   struct stat st;
   int r;
#ifdef USE_MMAP
   int fd;
#else
   FILE *f;
#endif
   IF_DEBUG(linker, debugBelch("loadObj %s\n", path));
   initLinker();

   /* debugBelch("loadObj %s\n", path ); */

   /* Check that we haven't already loaded this object.
      Ignore requests to load multiple times */
   {
       ObjectCode *o;
       int is_dup = 0;
       for (o = objects; o; o = o->next) {
          if (0 == strcmp(o->fileName, path)) {
             is_dup = 1;
             break; /* don't need to search further */
          }
       }
       if (is_dup) {
          IF_DEBUG(linker, debugBelch(
            "GHCi runtime linker: warning: looks like you're trying to load the\n"
            "same object file twice:\n"
            "   %s\n"
            "GHCi will ignore this, but be warned.\n"
            , path));
          return 1; /* success */
       }
   }

   oc = stgMallocBytes(sizeof(ObjectCode), "loadObj(oc)");

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

   r = stat(path, &st);
   if (r == -1) {
       IF_DEBUG(linker, debugBelch("File doesn't exist\n"));
       return 0;
   }

   /* sigh, strdup() isn't a POSIX function, so do it the long way */
   oc->fileName = stgMallocBytes( strlen(path)+1, "loadObj" );
   strcpy(oc->fileName, path);

   oc->fileSize          = st.st_size;
   oc->symbols           = NULL;
   oc->sections          = NULL;
   oc->proddables        = NULL;

   /* chain it onto the list of objects */
   oc->next              = objects;
   objects               = oc;

#ifdef USE_MMAP
   /* On many architectures malloc'd memory isn't executable, so we need to use mmap. */

#if defined(openbsd_HOST_OS)
   fd = open(path, O_RDONLY, S_IRUSR);
#else
   fd = open(path, O_RDONLY);
#endif
   if (fd == -1)
      barf("loadObj: can't open `%s'", path);

   oc->image = mmapForLinker(oc->fileSize, 0, fd);

   close(fd);

#else /* !USE_MMAP */
   /* load the image into memory */
   f = fopen(path, "rb");
   if (!f)
       barf("loadObj: can't read `%s'", path);

#   if defined(mingw32_HOST_OS)
	// TODO: We would like to use allocateExec here, but allocateExec
	//       cannot currently allocate blocks large enough.
    oc->image = VirtualAlloc(NULL, oc->fileSize, MEM_RESERVE | MEM_COMMIT,
                             PAGE_EXECUTE_READWRITE);
#   elif defined(darwin_HOST_OS)
    // In a Mach-O .o file, all sections can and will be misaligned
    // if the total size of the headers is not a multiple of the
    // desired alignment. This is fine for .o files that only serve
    // as input for the static linker, but it's not fine for us,
    // as SSE (used by gcc for floating point) and Altivec require
    // 16-byte alignment.
    // We calculate the correct alignment from the header before
    // reading the file, and then we misalign oc->image on purpose so
    // that the actual sections end up aligned again.
   oc->misalignment = machoGetMisalignment(f);
   oc->image = stgMallocBytes(oc->fileSize + oc->misalignment, "loadObj(image)");
   oc->image += oc->misalignment;
#  else
   oc->image = stgMallocBytes(oc->fileSize, "loadObj(image)");
#  endif

   {
       int n;
       n = fread ( oc->image, 1, oc->fileSize, f );
       if (n != oc->fileSize)
           barf("loadObj: error whilst reading `%s'", path);
   }
   fclose(f);
#endif /* USE_MMAP */

#  if defined(OBJFORMAT_MACHO) && (defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH))
   r = ocAllocateSymbolExtras_MachO ( oc );
   if (!r) {
       IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO failed\n"));
       return r;
   }
#  elif defined(OBJFORMAT_ELF) && (defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH))
   r = ocAllocateSymbolExtras_ELF ( oc );
   if (!r) {
       IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_ELF failed\n"));
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
       IF_DEBUG(linker, debugBelch("ocVerifyImage_* failed\n"));
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
       IF_DEBUG(linker, debugBelch("ocGetNames_* failed\n"));
       return r;
   }

   /* loaded, but not resolved yet */
   oc->status = OBJECT_LOADED;

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
	    oc->status = OBJECT_RESOLVED;
	}
    }
    return 1;
}

/* -----------------------------------------------------------------------------
 * delete an object from the pool
 */
HsInt
unloadObj( char *path )
{
    ObjectCode *oc, *prev;

    ASSERT(symhash != NULL);
    ASSERT(objects != NULL);

    initLinker();

    prev = NULL;
    for (oc = objects; oc; prev = oc, oc = oc->next) {
	if (!strcmp(oc->fileName,path)) {

	    /* Remove all the mappings for the symbols within this
	     * object..
	     */
	    {
                int i;
                for (i = 0; i < oc->n_symbols; i++) {
                   if (oc->symbols[i] != NULL) {
                       removeStrHashTable(symhash, oc->symbols[i], NULL);
                   }
                }
            }

	    if (prev == NULL) {
		objects = oc->next;
	    } else {
		prev->next = oc->next;
	    }

	    // We're going to leave this in place, in case there are
	    // any pointers from the heap into it:
		// #ifdef mingw32_HOST_OS
		//  VirtualFree(oc->image);
		// #else
	    //  stgFree(oc->image);
	    // #endif
	    stgFree(oc->fileName);
	    stgFree(oc->symbols);
	    stgFree(oc->sections);
	    stgFree(oc);
	    return 1;
	}
    }

    errorBelch("unloadObj: can't find `%s' to unload", path);
    return 0;
}

/* -----------------------------------------------------------------------------
 * Sanity checking.  For each ObjectCode, maintain a list of address ranges
 * which may be prodded during relocation, and abort if we try and write
 * outside any of these.
 */
static void addProddableBlock ( ObjectCode* oc, void* start, int size )
{
   ProddableBlock* pb
      = stgMallocBytes(sizeof(ProddableBlock), "addProddableBlock");
   /* debugBelch("aPB %p %p %d\n", oc, start, size); */
   ASSERT(size > 0);
   pb->start      = start;
   pb->size       = size;
   pb->next       = oc->proddables;
   oc->proddables = pb;
}

static void checkProddableBlock ( ObjectCode* oc, void* addr )
{
   ProddableBlock* pb;
   for (pb = oc->proddables; pb != NULL; pb = pb->next) {
      char* s = (char*)(pb->start);
      char* e = s + pb->size - 1;
      char* a = (char*)addr;
      /* Assumes that the biggest fixup involves a 4-byte write.  This
         probably needs to be changed to 8 (ie, +7) on 64-bit
         plats. */
      if (a >= s && (a+3) <= e) return;
   }
   barf("checkProddableBlock: invalid fixup in runtime linker");
}

/* -----------------------------------------------------------------------------
 * Section management.
 */
static void addSection ( ObjectCode* oc, SectionKind kind,
                         void* start, void* end )
{
   Section* s   = stgMallocBytes(sizeof(Section), "addSection");
   s->start     = start;
   s->end       = end;
   s->kind      = kind;
   s->next      = oc->sections;
   oc->sections = s;
   /*
   debugBelch("addSection: %p-%p (size %d), kind %d\n",
                   start, ((char*)end)-1, end - start + 1, kind );
   */
}


/* --------------------------------------------------------------------------
 * Symbol Extras.
 * This is about allocating a small chunk of memory for every symbol in the
 * object file. We make sure that the SymboLExtras are always "in range" of
 * limited-range PC-relative instructions on various platforms by allocating
 * them right next to the object code itself.
 */

#if defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)

/*
  ocAllocateSymbolExtras

  Allocate additional space at the end of the object file image to make room
  for jump islands (powerpc, x86_64) and GOT entries (x86_64).
  
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
        oc->symbol_extras = mmapForLinker(sizeof(SymbolExtra) * count, 
                                          MAP_ANONYMOUS, -1);
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

static void ocFlushInstructionCache( ObjectCode *oc )
{
    int n = (oc->fileSize + sizeof( SymbolExtra ) * oc->n_symbol_extras + 3) / 4;
    unsigned long *p = (unsigned long *) oc->image;

    while( n-- )
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
#endif

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



typedef unsigned char  UChar;
typedef unsigned short UInt16;
typedef unsigned int   UInt32;
typedef          int   Int32;


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
      (which is never freed ...)
   */
   newstr = stgMallocBytes(9, "cstring_from_COFF_symbol_name");
   ASSERT(newstr);
   strncpy((char*)newstr,(char*)name,8);
   newstr[8] = 0;
   return newstr;
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

   if (hdr->Machine != 0x14c) {
      errorBelch("%s: Not x86 PEi386", oc->fileName);
      return 0;
   }
   if (hdr->SizeOfOptionalHeader != 0) {
      errorBelch("%s: PEi386 with nonempty optional header", oc->fileName);
      return 0;
   }
   if ( /* (hdr->Characteristics & MYIMAGE_FILE_RELOCS_STRIPPED) || */
        (hdr->Characteristics & MYIMAGE_FILE_EXECUTABLE_IMAGE) ||
        (hdr->Characteristics & MYIMAGE_FILE_DLL) ||
        (hdr->Characteristics & MYIMAGE_FILE_SYSTEM) ) {
      errorBelch("%s: Not a PEi386 object file", oc->fileName);
      return 0;
   }
   if ( (hdr->Characteristics & MYIMAGE_FILE_BYTES_REVERSED_HI)
        /* || !(hdr->Characteristics & MYIMAGE_FILE_32BIT_MACHINE) */ ) {
      errorBelch("%s: Invalid PEi386 word size or endiannness: %d",
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

   debugBelch( "sectab offset = %d\n", ((UChar*)sectab) - ((UChar*)hdr) );
   debugBelch( "symtab offset = %d\n", ((UChar*)symtab) - ((UChar*)hdr) );
   debugBelch( "strtab offset = %d\n", ((UChar*)strtab) - ((UChar*)hdr) );

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
      if (0 != strcmp((char*)sectab_i->Name, ".bss")) continue;
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
       * variable decls into to the .bss section. (The specific function in Q which
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
      IF_DEBUG(linker, debugBelch("section name = %s\n", sectab_i->Name ));

#     if 0
      /* I'm sure this is the Right Way to do it.  However, the
         alternative of testing the sectab_i->Name field seems to
         work ok with Cygwin.
      */
      if (sectab_i->Characteristics & MYIMAGE_SCN_CNT_CODE ||
          sectab_i->Characteristics & MYIMAGE_SCN_CNT_INITIALIZED_DATA)
         kind = SECTIONKIND_CODE_OR_RODATA;
#     endif

      if (0==strcmp(".text",(char*)sectab_i->Name) ||
          0==strcmp(".rdata",(char*)sectab_i->Name)||
          0==strcmp(".rodata",(char*)sectab_i->Name))
         kind = SECTIONKIND_CODE_OR_RODATA;
      if (0==strcmp(".data",(char*)sectab_i->Name) ||
          0==strcmp(".bss",(char*)sectab_i->Name))
         kind = SECTIONKIND_RWDATA;

      ASSERT(sectab_i->SizeOfRawData == 0 || sectab_i->VirtualSize == 0);
      sz = sectab_i->SizeOfRawData;
      if (sz < sectab_i->VirtualSize) sz = sectab_i->VirtualSize;

      start = ((UChar*)(oc->image)) + sectab_i->PointerToRawData;
      end   = start + sz - 1;

      if (kind == SECTIONKIND_OTHER
          /* Ignore sections called which contain stabs debugging
             information. */
          && 0 != strcmp(".stab", (char*)sectab_i->Name)
          && 0 != strcmp(".stabstr", (char*)sectab_i->Name)
          /* ignore constructor section for now */
          && 0 != strcmp(".ctors", (char*)sectab_i->Name)
          /* ignore section generated from .ident */
          && 0!= strcmp("/4", (char*)sectab_i->Name)
	  /* ignore unknown section that appeared in gcc 3.4.5(?) */
          && 0!= strcmp(".reloc", (char*)sectab_i->Name)
         ) {
         errorBelch("Unknown PEi386 section name `%s' (while processing: %s)", sectab_i->Name, oc->fileName);
         return 0;
      }

      if (kind != SECTIONKIND_OTHER && end >= start) {
         addSection(oc, kind, start, end);
         addProddableBlock(oc, start, end - start + 1);
      }
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
         ghciInsertStrHashTable(oc->fileName, symhash, (char*)sname, addr);
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
   UInt32        S;
   UInt32*       pP;

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

      /* Ignore sections called which contain stabs debugging
         information. */
      if (0 == strcmp(".stab", (char*)sectab_i->Name)
          || 0 == strcmp(".stabstr", (char*)sectab_i->Name)
          || 0 == strcmp(".ctors", (char*)sectab_i->Name))
         continue;

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
         pP = (UInt32*)(
                 ((UChar*)(oc->image))
                 + (sectab_i->PointerToRawData
                    + reltab_j->VirtualAddress
                    - sectab_i->VirtualAddress )
              );
         /* the existing contents of pP */
         A = *pP;
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
               errorBelch("%s: can't find section `%s'", oc->fileName, sym->Name);
               return 0;
            }
            S = ((UInt32)(oc->image))
                + (section_sym->PointerToRawData
                   + sym->Value);
         } else {
            copyName ( sym->Name, strtab, symbol, 1000-1 );
            S = (UInt32) lookupSymbol( (char*)symbol );
            if ((void*)S != NULL) goto foundit;
            errorBelch("%s: unknown symbol `%s'", oc->fileName, symbol);
            return 0;
           foundit:;
         }
         checkProddableBlock(oc, pP);
         switch (reltab_j->Type) {
            case MYIMAGE_REL_I386_DIR32:
               *pP = A + S;
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
               *pP = S - ((UInt32)pP) - 4 + A;
               break;
            default:
               debugBelch("%s: unhandled PEi386 relocation type %d",
		     oc->fileName, reltab_j->Type);
               return 0;
         }

      }
   }

   IF_DEBUG(linker, debugBelch("completed %s", oc->fileName));
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

static char *
findElfSection ( void* objImage, Elf_Word sh_type )
{
   char* ehdrC = (char*)objImage;
   Elf_Ehdr* ehdr = (Elf_Ehdr*)ehdrC;
   Elf_Shdr* shdr = (Elf_Shdr*)(ehdrC + ehdr->e_shoff);
   char* sh_strtab = ehdrC + shdr[ehdr->e_shstrndx].sh_offset;
   char* ptr = NULL;
   int i;

   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type == sh_type
          /* Ignore the section header's string table. */
          && i != ehdr->e_shstrndx
	  /* Ignore string tables named .stabstr, as they contain
             debugging info. */
          && 0 != memcmp(".stabstr", sh_strtab + shdr[i].sh_name, 8)
         ) {
         ptr = ehdrC + shdr[i].sh_offset;
         break;
      }
   }
   return ptr;
}

static int
ocVerifyImage_ELF ( ObjectCode* oc )
{
   Elf_Shdr* shdr;
   Elf_Sym*  stab;
   int i, j, nent, nstrtab, nsymtabs;
   char* sh_strtab;
   char* strtab;

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

      if (shdr[i].sh_type == SHT_REL) {
	  IF_DEBUG(linker,debugBelch("Rel  " ));
      } else if (shdr[i].sh_type == SHT_RELA) {
	  IF_DEBUG(linker,debugBelch("RelA " ));
      } else {
	  IF_DEBUG(linker,debugBelch("     "));
      }
      if (sh_strtab) {
	  IF_DEBUG(linker,debugBelch("sname=%s\n", sh_strtab + shdr[i].sh_name ));
      }
   }

   IF_DEBUG(linker,debugBelch( "\nString tables" ));
   strtab = NULL;
   nstrtab = 0;
   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type == SHT_STRTAB
          /* Ignore the section header's string table. */
          && i != ehdr->e_shstrndx
	  /* Ignore string tables named .stabstr, as they contain
             debugging info. */
          && 0 != memcmp(".stabstr", sh_strtab + shdr[i].sh_name, 8)
         ) {
         IF_DEBUG(linker,debugBelch("   section %d is a normal string table", i ));
         strtab = ehdrC + shdr[i].sh_offset;
         nstrtab++;
      }
   }
   if (nstrtab != 1) {
      errorBelch("%s: no string tables, or too many", oc->fileName);
      return 0;
   }

   nsymtabs = 0;
   IF_DEBUG(linker,debugBelch( "\nSymbol tables" ));
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

         IF_DEBUG(linker,debugBelch("name=%s\n", strtab + stab[j].st_name ));
      }
   }

   if (nsymtabs == 0) {
      errorBelch("%s: didn't find any symbol tables", oc->fileName);
      return 0;
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
   int i, j, k, nent;
   Elf_Sym* stab;

   char*     ehdrC    = (char*)(oc->image);
   Elf_Ehdr* ehdr     = (Elf_Ehdr*)ehdrC;
   char*     strtab   = findElfSection ( ehdrC, SHT_STRTAB );
   Elf_Shdr* shdr     = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);

   ASSERT(symhash != NULL);

   if (!strtab) {
      errorBelch("%s: no strtab", oc->fileName);
      return 0;
   }

   k = 0;
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
      nent = shdr[i].sh_size / sizeof(Elf_Sym);

      oc->n_symbols = nent;
      oc->symbols = stgMallocBytes(oc->n_symbols * sizeof(char*),
                                   "ocGetNames_ELF(oc->symbols)");

      for (j = 0; j < nent; j++) {

         char  isLocal = FALSE; /* avoids uninit-var warning */
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
            } else {
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
               ghciInsertStrHashTable(oc->fileName, symhash, nm, ad);
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
   relocations appear to be of this form. */
static int
do_Elf_Rel_relocations ( ObjectCode* oc, char* ehdrC,
                         Elf_Shdr* shdr, int shnum,
                         Elf_Sym*  stab, char* strtab )
{
   int j;
   char *symbol;
   Elf_Word* targ;
   Elf_Rel*  rtab = (Elf_Rel*) (ehdrC + shdr[shnum].sh_offset);
   int         nent = shdr[shnum].sh_size / sizeof(Elf_Rel);
   int target_shndx = shdr[shnum].sh_info;
   int symtab_shndx = shdr[shnum].sh_link;

   stab  = (Elf_Sym*) (ehdrC + shdr[ symtab_shndx ].sh_offset);
   targ  = (Elf_Word*)(ehdrC + shdr[ target_shndx ].sh_offset);
   IF_DEBUG(linker,debugBelch( "relocations for section %d using symtab %d\n",
                          target_shndx, symtab_shndx ));

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
      Elf_Addr  A  = *pP;
      Elf_Addr  S;
      void*     S_tmp;
      Elf_Addr  value;
      StgStablePtr stablePtr;
      StgPtr stableVal;

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
            stablePtr = (StgStablePtr)lookupHashTable(stablehash, (StgWord)symbol);
            if (NULL == stablePtr) {
              /* No, so look up the name in our global table. */
              S_tmp = lookupSymbol( symbol );
              S = (Elf_Addr)S_tmp;
            } else {
              stableVal = deRefStablePtr( stablePtr );
              S_tmp = stableVal;
              S = (Elf_Addr)S_tmp;
            }
	 }
         if (!S) {
            errorBelch("%s: unknown symbol `%s'", oc->fileName, symbol);
	    return 0;
         }
         IF_DEBUG(linker,debugBelch( "`%s' resolves to %p\n", symbol, (void*)S ));
      }

      IF_DEBUG(linker,debugBelch( "Reloc: P = %p   S = %p   A = %p\n",
			     (void*)P, (void*)S, (void*)A ));
      checkProddableBlock ( oc, pP );

      value = S + A;

      switch (ELF_R_TYPE(info)) {
#        ifdef i386_HOST_ARCH
         case R_386_32:   *pP = value;     break;
         case R_386_PC32: *pP = value - P; break;
#        endif
         default:
            errorBelch("%s: unhandled ELF relocation(Rel) type %lu\n",
		  oc->fileName, (lnat)ELF_R_TYPE(info));
            return 0;
      }

   }
   return 1;
}

/* Do ELF relocations for which explicit addends are supplied.
   sparc-solaris relocations appear to be of this form. */
static int
do_Elf_Rela_relocations ( ObjectCode* oc, char* ehdrC,
                          Elf_Shdr* shdr, int shnum,
                          Elf_Sym*  stab, char* strtab )
{
   int j;
   char *symbol = NULL;
   Elf_Addr targ;
   Elf_Rela* rtab = (Elf_Rela*) (ehdrC + shdr[shnum].sh_offset);
   int         nent = shdr[shnum].sh_size / sizeof(Elf_Rela);
   int target_shndx = shdr[shnum].sh_info;
   int symtab_shndx = shdr[shnum].sh_link;

   stab  = (Elf_Sym*) (ehdrC + shdr[ symtab_shndx ].sh_offset);
   targ  = (Elf_Addr) (ehdrC + shdr[ target_shndx ].sh_offset);
   IF_DEBUG(linker,debugBelch( "relocations for section %d using symtab %d\n",
                          target_shndx, symtab_shndx ));

   for (j = 0; j < nent; j++) {
#if defined(DEBUG) || defined(sparc_HOST_ARCH) || defined(ia64_HOST_ARCH) || defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
      /* This #ifdef only serves to avoid unused-var warnings. */
      Elf_Addr  offset = rtab[j].r_offset;
      Elf_Addr  P      = targ + offset;
#endif
      Elf_Addr  info   = rtab[j].r_info;
      Elf_Addr  A      = rtab[j].r_addend;
      Elf_Addr  S;
      void*     S_tmp;
      Elf_Addr  value;
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

      value = S + A;

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
            w2	= (Elf_Word)value;

            // SPARC doesn't do misaligned writes of 32 bit words,
	    //       so we have to do this one byte-at-a-time.
	    char *pPc 	= (char*)pP;
	    pPc[0]	= (char) ((Elf_Word)(w2 & 0xff000000) >> 24);
	    pPc[1]	= (char) ((Elf_Word)(w2 & 0x00ff0000) >> 16);
	    pPc[2]	= (char) ((Elf_Word)(w2 & 0x0000ff00) >> 8);
	    pPc[3]	= (char) ((Elf_Word)(w2 & 0x000000ff));
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
	  break;
      }

      case R_X86_64_PC64:
      {
	  StgInt64 off = value - P;
	  *(Elf64_Word *)P = (Elf64_Word)off;
	  break;
      }

      case R_X86_64_32:
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
	  break;

      case R_X86_64_32S:
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
	  break;
	  
      case R_X86_64_GOTPCREL:
      {
          StgInt64 gotAddress = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)->addr;
	  StgInt64 off = gotAddress + A - P;
	  *(Elf64_Word *)P = (Elf64_Word)off;
	  break;
      }
      
      case R_X86_64_PLT32:
      {
	  StgInt64 off = value - P;
	  if (off >= 0x7fffffffL || off < -0x80000000L) {
              StgInt64 pltAddress = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                                    -> jumpIsland;
              off = pltAddress + A - P;
	  }
	  *(Elf64_Word *)P = (Elf64_Word)off;
	  break;
      }
#endif

         default:
            errorBelch("%s: unhandled ELF relocation(RelA) type %lu\n",
		  oc->fileName, (lnat)ELF_R_TYPE(info));
            return 0;
      }

   }
   return 1;
}

static int
ocResolve_ELF ( ObjectCode* oc )
{
   char *strtab;
   int   shnum, ok;
   Elf_Sym*  stab  = NULL;
   char*     ehdrC = (char*)(oc->image);
   Elf_Ehdr* ehdr  = (Elf_Ehdr*) ehdrC;
   Elf_Shdr* shdr  = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);

   /* first find "the" symbol table */
   stab = (Elf_Sym*) findElfSection ( ehdrC, SHT_SYMTAB );

   /* also go find the string table */
   strtab = findElfSection ( ehdrC, SHT_STRTAB );

   if (stab == NULL || strtab == NULL) {
      errorBelch("%s: can't find string or symbol table", oc->fileName);
      return 0;
   }

   /* Process the relocation sections. */
   for (shnum = 0; shnum < ehdr->e_shnum; shnum++) {
      if (shdr[shnum].sh_type == SHT_REL) {
         ok = do_Elf_Rel_relocations ( oc, ehdrC, shdr,
                                       shnum, stab, strtab );
         if (!ok) return ok;
      }
      else
      if (shdr[shnum].sh_type == SHT_RELA) {
         ok = do_Elf_Rela_relocations ( oc, ehdrC, shdr,
                                        shnum, stab, strtab );
         if (!ok) return ok;
      }
   }

#if defined(powerpc_HOST_ARCH)
   ocFlushInstructionCache( oc );
#endif

   return 1;
}

/*
 * PowerPC & X86_64 ELF specifics
 */

#if defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)

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
    errorBelch( "This ELF file contains no symtab" );
    return 0;
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
static int ocAllocateSymbolExtras_MachO(ObjectCode* oc)
{
    struct mach_header *header = (struct mach_header *) oc->image;
    struct load_command *lc = (struct load_command *) (header + 1);
    unsigned i;

    for( i = 0; i < header->ncmds; i++ )
    {   
        if( lc->cmd == LC_SYMTAB )
        {
                // Find out the first and last undefined external
                // symbol, so we don't have to allocate too many
                // jump islands.
            struct symtab_command *symLC = (struct symtab_command *) lc;
            unsigned min = symLC->nsyms, max = 0;
            struct nlist *nlist =
                symLC ? (struct nlist*) ((char*) oc->image + symLC->symoff)
                      : NULL;
            for(i=0;i<symLC->nsyms;i++)
            {
                if(nlist[i].n_type & N_STAB)
                    ;
                else if(nlist[i].n_type & N_EXT)
                {
                    if((nlist[i].n_type & N_TYPE) == N_UNDF
                        && (nlist[i].n_value == 0))
                    {
                        if(i < min)
                            min = i;
                        if(i > max)
                            max = i;
                    }
                }
            }
            if(max >= min)
                return ocAllocateSymbolExtras(oc, max - min + 1, min);

            break;
        }
        
        lc = (struct load_command *) ( ((char *)lc) + lc->cmdsize );
    }
    return ocAllocateSymbolExtras(oc,0,0);
}
#endif
#ifdef x86_64_HOST_ARCH
static int ocAllocateSymbolExtras_MachO(ObjectCode* oc)
{
    struct mach_header *header = (struct mach_header *) oc->image;
    struct load_command *lc = (struct load_command *) (header + 1);
    unsigned i;

    for( i = 0; i < header->ncmds; i++ )
    {   
        if( lc->cmd == LC_SYMTAB )
        {
                // Just allocate one entry for every symbol
            struct symtab_command *symLC = (struct symtab_command *) lc;
            
            return ocAllocateSymbolExtras(oc, symLC->nsyms, 0);
        }
        
        lc = (struct load_command *) ( ((char *)lc) + lc->cmdsize );
    }
    return ocAllocateSymbolExtras(oc,0,0);
}
#endif

static int ocVerifyImage_MachO(ObjectCode* oc)
{
    char *image = (char*) oc->image;
    struct mach_header *header = (struct mach_header*) image;

#if x86_64_HOST_ARCH || powerpc64_HOST_ARCH
    if(header->magic != MH_MAGIC_64)
        return 0;
#else
    if(header->magic != MH_MAGIC)
        return 0;
#endif
    // FIXME: do some more verifying here
    return 1;
}

static int resolveImports(
    ObjectCode* oc,
    char *image,
    struct symtab_command *symLC,
    struct section *sect,    // ptr to lazy or non-lazy symbol pointer section
    unsigned long *indirectSyms,
    struct nlist *nlist)
{
    unsigned i;
    size_t itemSize = 4;

#if i386_HOST_ARCH
    int isJumpTable = 0;
    if(!strcmp(sect->sectname,"__jump_table"))
    {
        isJumpTable = 1;
        itemSize = 5;
        ASSERT(sect->reserved2 == itemSize);
    }
#endif

    for(i=0; i*itemSize < sect->size;i++)
    {
	// according to otool, reserved1 contains the first index into the indirect symbol table
	struct nlist *symbol = &nlist[indirectSyms[sect->reserved1+i]];
	char *nm = image + symLC->stroff + symbol->n_un.n_strx;
	void *addr = NULL;

	if((symbol->n_type & N_TYPE) == N_UNDF
	    && (symbol->n_type & N_EXT) && (symbol->n_value != 0))
	    addr = (void*) (symbol->n_value);
	else
	    addr = lookupSymbol(nm);
	if(!addr)
	{
	    errorBelch("\n%s: unknown symbol `%s'", oc->fileName, nm);
	    return 0;
	}
	ASSERT(addr);

#if i386_HOST_ARCH
        if(isJumpTable)
        {
            checkProddableBlock(oc,image + sect->offset + i*itemSize);
            *(image + sect->offset + i*itemSize) = 0xe9; // jmp
            *(unsigned*)(image + sect->offset + i*itemSize + 1)
                = (char*)addr - (image + sect->offset + i*itemSize + 5);
        }
        else
#endif
	{
	    checkProddableBlock(oc,((void**)(image + sect->offset)) + i);
	    ((void**)(image + sect->offset))[i] = addr;
        }
    }

    return 1;
}

static unsigned long relocateAddress(
    ObjectCode* oc,
    int nSections,
    struct section* sections,
    unsigned long address)
{
    int i;
    for(i = 0; i < nSections; i++)
    {
        if(sections[i].addr <= address
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

static int relocateSection(
    ObjectCode* oc,
    char *image,
    struct symtab_command *symLC, struct nlist *nlist,
    int nSections, struct section* sections, struct section *sect)
{
    struct relocation_info *relocs;
    int i,n;

    if(!strcmp(sect->sectname,"__la_symbol_ptr"))
	return 1;
    else if(!strcmp(sect->sectname,"__nl_symbol_ptr"))
	return 1;
    else if(!strcmp(sect->sectname,"__la_sym_ptr2"))
	return 1;
    else if(!strcmp(sect->sectname,"__la_sym_ptr3"))
	return 1;

    n = sect->nreloc;
    relocs = (struct relocation_info*) (image + sect->reloff);

    for(i=0;i<n;i++)
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
        
        checkProddableBlock(oc,thingPtr);
        switch(reloc->r_length)
        {
            case 0:
                thing = *(uint8_t*)thingPtr;
                baseValue = (uint64_t)thingPtr + 1;
                break;
            case 1:
                thing = *(uint16_t*)thingPtr;
                baseValue = (uint64_t)thingPtr + 2;
                break;
            case 2:
                thing = *(uint32_t*)thingPtr;
                baseValue = (uint64_t)thingPtr + 4;
                break;
            case 3:
                thing = *(uint64_t*)thingPtr;
                baseValue = (uint64_t)thingPtr + 8;
                break;
            default:
                barf("Unknown size.");
        }
        
        if(type == X86_64_RELOC_GOT
           || type == X86_64_RELOC_GOT_LOAD)
        {
            ASSERT(reloc->r_extern);
            value = (uint64_t) &makeSymbolExtra(oc, reloc->r_symbolnum, value)->addr;
            
            type = X86_64_RELOC_SIGNED;
        }
        else if(reloc->r_extern)
        {
            struct nlist *symbol = &nlist[reloc->r_symbolnum];
            char *nm = image + symLC->stroff + symbol->n_un.n_strx;
            if(symbol->n_value == 0)
                value = (uint64_t) lookupSymbol(nm);
            else
                value = relocateAddress(oc, nSections, sections,
                                        symbol->n_value);
        }
        else
        {
            value = sections[reloc->r_symbolnum-1].offset
                  - sections[reloc->r_symbolnum-1].addr
		  + (uint64_t) image;
        }
        
        if(type == X86_64_RELOC_BRANCH)
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
		    checkProddableBlock(oc,wordPtr);

                    // Note on relocation types:
                    // i386 uses the GENERIC_RELOC_* types,
                    // while ppc uses special PPC_RELOC_* types.
                    // *_RELOC_VANILLA and *_RELOC_PAIR have the same value
                    // in both cases, all others are different.
                    // Therefore, we use GENERIC_RELOC_VANILLA
                    // and GENERIC_RELOC_PAIR instead of the PPC variants,
                    // and use #ifdefs for the other types.
                    
		    // Step 1: Figure out what the relocated value should be
		    if(scat->r_type == GENERIC_RELOC_VANILLA)
		    {
                        word = *wordPtr + (unsigned long) relocateAddress(
                                                                oc,
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

		        if(!pair->r_scattered || pair->r_type != GENERIC_RELOC_PAIR)
		            barf("Invalid Mach-O file: "
		                 "RELOC_*_SECTDIFF not followed by RELOC_PAIR");

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
                        if((pair->r_address & R_SCATTERED) || pair->r_type != PPC_RELOC_PAIR)
		            barf("Invalid Mach-O file: "
		                 "PPC_RELOC_* not followed by PPC_RELOC_PAIR");
                        
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
                    else
                    {
		        barf ("Don't know how to handle this Mach-O "
		              "scattered relocation entry: "
                              "object file %s; entry type %ld; "
                              "address %#lx\n", 
                              oc->fileName, scat->r_type, scat->r_address);
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
                    else if(scat->r_type == PPC_RELOC_LO16_SECTDIFF || scat->r_type == PPC_RELOC_LO16)
                    {
                        ((unsigned short*) wordPtr)[1] = word & 0xFFFF;
                    }
                    else if(scat->r_type == PPC_RELOC_HI16_SECTDIFF || scat->r_type == PPC_RELOC_HI16)
                    {
                        ((unsigned short*) wordPtr)[1] = (word >> 16) & 0xFFFF;
                    }
                    else if(scat->r_type == PPC_RELOC_HA16_SECTDIFF || scat->r_type == PPC_RELOC_HA16)
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
                         oc->fileName, scat->r_type, scat->r_length,
                         scat->r_address);
                    return 0;
		}
	    }
	    else /* scat->r_pcrel */
	    {
	        barf("Don't know how to handle *PC-relative* Mach-O "
	             "scattered relocation entry: "
                     "object file %s; entry type %ld; address %#lx\n", 
                     oc->fileName, scat->r_type, scat->r_address);
               return 0;
	    }

	}
	else /* !(relocs[i].r_address & R_SCATTERED) */
	{
	    struct relocation_info *reloc = &relocs[i];
	    if(reloc->r_pcrel && !reloc->r_extern)
		continue;

	    if(reloc->r_length == 2)
	    {
		unsigned long word = 0;
#ifdef powerpc_HOST_ARCH
                unsigned long jumpIsland = 0;
                long offsetToJumpIsland = 0xBADBAD42; // initialise to bad value
                                                      // to avoid warning and to catch
                                                      // bugs.
#endif

		unsigned long* wordPtr = (unsigned long*) (image + sect->offset + reloc->r_address);
		checkProddableBlock(oc,wordPtr);

		if(reloc->r_type == GENERIC_RELOC_VANILLA)
		{
		    word = *wordPtr;
		}
#ifdef powerpc_HOST_ARCH
		else if(reloc->r_type == PPC_RELOC_LO16)
		{
		    word = ((unsigned short*) wordPtr)[1];
		    word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF) << 16;
		}
		else if(reloc->r_type == PPC_RELOC_HI16)
		{
		    word = ((unsigned short*) wordPtr)[1] << 16;
		    word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF);
		}
		else if(reloc->r_type == PPC_RELOC_HA16)
		{
		    word = ((unsigned short*) wordPtr)[1] << 16;
		    word += ((short)relocs[i+1].r_address & (short)0xFFFF);
		}
		else if(reloc->r_type == PPC_RELOC_BR24)
		{
		    word = *wordPtr;
		    word = (word & 0x03FFFFFC) | ((word & 0x02000000) ? 0xFC000000 : 0);
		}
#endif
                else
                {
                    barf("Can't handle this Mach-O relocation entry "
		         "(not scattered): "
                         "object file %s; entry type %ld; address %#lx\n", 
                         oc->fileName, reloc->r_type, reloc->r_address);
                    return 0;
                }

		if(!reloc->r_extern)
		{
		    long delta =
			sections[reloc->r_symbolnum-1].offset
			- sections[reloc->r_symbolnum-1].addr
			+ ((long) image);

		    word += delta;
		}
		else
		{
		    struct nlist *symbol = &nlist[reloc->r_symbolnum];
		    char *nm = image + symLC->stroff + symbol->n_un.n_strx;
		    void *symbolAddress = lookupSymbol(nm);
		    if(!symbolAddress)
		    {
			errorBelch("\nunknown symbol `%s'", nm);
			return 0;
		    }

		    if(reloc->r_pcrel)
                    {  
#ifdef powerpc_HOST_ARCH
                            // In the .o file, this should be a relative jump to NULL
                            // and we'll change it to a relative jump to the symbol
                        ASSERT(word + reloc->r_address == 0);
                        jumpIsland = (unsigned long)
                                        &makeSymbolExtra(oc,
                                                         reloc->r_symbolnum,
                                                         (unsigned long) symbolAddress)
                                         -> jumpIsland;
                        if(jumpIsland != 0)
                        {
                            offsetToJumpIsland = word + jumpIsland
                                - (((long)image) + sect->offset - sect->addr);
                        }
#endif
			word += (unsigned long) symbolAddress
                                - (((long)image) + sect->offset - sect->addr);
                    }
                    else
                    {
                        word += (unsigned long) symbolAddress;
                    }
		}

		if(reloc->r_type == GENERIC_RELOC_VANILLA)
		{
		    *wordPtr = word;
		    continue;
		}
#ifdef powerpc_HOST_ARCH
		else if(reloc->r_type == PPC_RELOC_LO16)
		{
		    ((unsigned short*) wordPtr)[1] = word & 0xFFFF;
		    i++; continue;
		}
		else if(reloc->r_type == PPC_RELOC_HI16)
		{
		    ((unsigned short*) wordPtr)[1] = (word >> 16) & 0xFFFF;
		    i++; continue;
		}
		else if(reloc->r_type == PPC_RELOC_HA16)
		{
		    ((unsigned short*) wordPtr)[1] = ((word >> 16) & 0xFFFF)
			+ ((word & (1<<15)) ? 1 : 0);
		    i++; continue;
		}
		else if(reloc->r_type == PPC_RELOC_BR24)
		{
                    if((long)word > (long)0x01FFFFFF || (long)word < (long)0xFFE00000)
                    {
                        // The branch offset is too large.
                        // Therefore, we try to use a jump island.
                        if(jumpIsland == 0)
                        {
                            barf("unconditional relative branch out of range: "
                                 "no jump island available");
                        }
                        
                        word = offsetToJumpIsland;
                        if((long)word > (long)0x01FFFFFF || (long)word < (long)0xFFE00000)
                            barf("unconditional relative branch out of range: "
                                 "jump island out of range");
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
                      oc->fileName, reloc->r_type, reloc->r_length,
                      reloc->r_address);
	         return 0;
	    }
	}
#endif
    }
    return 1;
}

static int ocGetNames_MachO(ObjectCode* oc)
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

    for(i=0;i<header->ncmds;i++)
    {
	if(lc->cmd == LC_SEGMENT || lc->cmd == LC_SEGMENT_64)
	    segLC = (struct segment_command*) lc;
	else if(lc->cmd == LC_SYMTAB)
	    symLC = (struct symtab_command*) lc;
	lc = (struct load_command *) ( ((char*)lc) + lc->cmdsize );
    }

    sections = (struct section*) (segLC+1);
    nlist = symLC ? (struct nlist*) (image + symLC->symoff)
                  : NULL;
    
    if(!segLC)
        barf("ocGetNames_MachO: no segment load command");

    for(i=0;i<segLC->nsects;i++)
    {
        if(sections[i].size == 0)
            continue;

        if((sections[i].flags & SECTION_TYPE) == S_ZEROFILL)
        {
            char * zeroFillArea = stgCallocBytes(1,sections[i].size,
                                      "ocGetNames_MachO(common symbols)");
            sections[i].offset = zeroFillArea - image;
        }

	if(!strcmp(sections[i].sectname,"__text"))
	    addSection(oc, SECTIONKIND_CODE_OR_RODATA,
		(void*) (image + sections[i].offset),
		(void*) (image + sections[i].offset + sections[i].size));
	else if(!strcmp(sections[i].sectname,"__const"))
	    addSection(oc, SECTIONKIND_RWDATA,
		(void*) (image + sections[i].offset),
		(void*) (image + sections[i].offset + sections[i].size));
	else if(!strcmp(sections[i].sectname,"__data"))
	    addSection(oc, SECTIONKIND_RWDATA,
		(void*) (image + sections[i].offset),
		(void*) (image + sections[i].offset + sections[i].size));
	else if(!strcmp(sections[i].sectname,"__bss")
	        || !strcmp(sections[i].sectname,"__common"))
	    addSection(oc, SECTIONKIND_RWDATA,
		(void*) (image + sections[i].offset),
		(void*) (image + sections[i].offset + sections[i].size));

        addProddableBlock(oc, (void*) (image + sections[i].offset),
                                        sections[i].size);
    }

	// count external symbols defined here
    oc->n_symbols = 0;
    if(symLC)
    {
        for(i=0;i<symLC->nsyms;i++)
        {
            if(nlist[i].n_type & N_STAB)
                ;
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
                    if((nlist[i].n_desc & N_WEAK_DEF) && lookupSymbol(nm))
                        ; // weak definition, and we already have a definition
                    else
                    {
                            ghciInsertStrHashTable(oc->fileName, symhash, nm,
                                                    image
                                                    + sections[nlist[i].n_sect-1].offset
                                                    - sections[nlist[i].n_sect-1].addr
                                                    + nlist[i].n_value);
                            oc->symbols[curSymbol++] = nm;
                    }
                }
            }
        }
    }

    commonStorage = stgCallocBytes(1,commonSize,"ocGetNames_MachO(common symbols)");
    commonCounter = (unsigned long)commonStorage;
    if(symLC)
    {
        for(i=0;i<symLC->nsyms;i++)
        {
	    if((nlist[i].n_type & N_TYPE) == N_UNDF
	            && (nlist[i].n_type & N_EXT) && (nlist[i].n_value != 0))
	    {
	        char *nm = image + symLC->stroff + nlist[i].n_un.n_strx;
	        unsigned long sz = nlist[i].n_value;

	        nlist[i].n_value = commonCounter;

	        ghciInsertStrHashTable(oc->fileName, symhash, nm,
	                               (void*)commonCounter);
	        oc->symbols[curSymbol++] = nm;

	        commonCounter += sz;
	    }
        }
    }
    return 1;
}

static int ocResolve_MachO(ObjectCode* oc)
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

    for(i=0;i<header->ncmds;i++)
    {
	if(lc->cmd == LC_SEGMENT || lc->cmd == LC_SEGMENT_64)
	    segLC = (struct segment_command*) lc;
	else if(lc->cmd == LC_SYMTAB)
	    symLC = (struct symtab_command*) lc;
	else if(lc->cmd == LC_DYSYMTAB)
	    dsymLC = (struct dysymtab_command*) lc;
	lc = (struct load_command *) ( ((char*)lc) + lc->cmdsize );
    }

    sections = (struct section*) (segLC+1);
    nlist = symLC ? (struct nlist*) (image + symLC->symoff)
                  : NULL;

    if(dsymLC)
    {
        unsigned long *indirectSyms
            = (unsigned long*) (image + dsymLC->indirectsymoff);

        for(i=0;i<segLC->nsects;i++)
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
        }
    }
    
    for(i=0;i<segLC->nsects;i++)
    {
	if(!relocateSection(oc,image,symLC,nlist,segLC->nsects,sections,&sections[i]))
	    return 0;
    }

#if defined (powerpc_HOST_ARCH)
    ocFlushInstructionCache( oc );
#endif

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

static void machoInitSymbolsWithoutUnderscore()
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
    ghciInsertStrHashTable("(GHCi built-in symbols)", symhash, #x, *p++);
    
    RTS_MACHO_NOUNDERLINE_SYMBOLS
    
#undef SymI_NeedsProto
}
#endif

#ifndef USE_MMAP
/*
 * Figure out by how much to shift the entire Mach-O file in memory
 * when loading so that its single segment ends up 16-byte-aligned
 */
static int machoGetMisalignment( FILE * f )
{
    struct mach_header header;
    int misalignment;
    
    fread(&header, sizeof(header), 1, f);
    rewind(f);

#if x86_64_HOST_ARCH || powerpc64_HOST_ARCH
    if(header.magic != MH_MAGIC_64)
        return 0;
#else
    if(header.magic != MH_MAGIC)
        return 0;
#endif

    misalignment = (header.sizeofcmds + sizeof(header))
                    & 0xF;

    return misalignment ? (16 - misalignment) : 0;
}
#endif

#endif

