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
#include "RtsFlags.h"
#include "HsFFI.h"
#include "Hash.h"
#include "Linker.h"
#include "LinkerInternals.h"
#include "RtsUtils.h"
#include "Schedule.h"
#include "Sparks.h"
#include "RtsTypeable.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include <stdlib.h>
#include <string.h>

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

#if defined(ia64_HOST_ARCH) || defined(openbsd_HOST_OS) || defined(linux_HOST_OS) || defined(freebsd_HOST_OS)
#define USE_MMAP
#include <fcntl.h>
#include <sys/mman.h>

#if defined(openbsd_HOST_OS) || defined(linux_HOST_OS) || defined(freebsd_HOST_OS)
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#endif

#endif

#if defined(linux_HOST_OS) || defined(solaris2_HOST_OS) || defined(freebsd_HOST_OS) || defined(netbsd_HOST_OS) || defined(openbsd_HOST_OS)
#  define OBJFORMAT_ELF
#elif defined(cygwin32_HOST_OS) || defined (mingw32_HOST_OS)
#  define OBJFORMAT_PEi386
#  include <windows.h>
#  include <math.h>
#elif defined(darwin_HOST_OS)
#  define OBJFORMAT_MACHO
#  include <mach-o/loader.h>
#  include <mach-o/nlist.h>
#  include <mach-o/reloc.h>
#  include <mach-o/dyld.h>
#if defined(powerpc_HOST_ARCH)
#  include <mach-o/ppc/reloc.h>
#endif
#endif

/* Hash table mapping symbol names to Symbol */
static /*Str*/HashTable *symhash;

typedef struct {
  void *addr;
} rootEntry;

/* Hash table mapping symbol names to StgStablePtr */
static /*Str*/HashTable *stablehash;
rootEntry *root_ptr_table = NULL;
static rootEntry *root_ptr_free = NULL;

static unsigned int RPT_size = 0;

/* List of currently loaded objects */
ObjectCode *objects = NULL;	/* initially empty */

#if defined(OBJFORMAT_ELF)
static int ocVerifyImage_ELF    ( ObjectCode* oc );
static int ocGetNames_ELF       ( ObjectCode* oc );
static int ocResolve_ELF        ( ObjectCode* oc );
#if defined(powerpc_HOST_ARCH)
static int ocAllocateJumpIslands_ELF ( ObjectCode* oc );
#endif
#elif defined(OBJFORMAT_PEi386)
static int ocVerifyImage_PEi386 ( ObjectCode* oc );
static int ocGetNames_PEi386    ( ObjectCode* oc );
static int ocResolve_PEi386     ( ObjectCode* oc );
#elif defined(OBJFORMAT_MACHO)
static int ocVerifyImage_MachO    ( ObjectCode* oc );
static int ocGetNames_MachO       ( ObjectCode* oc );
static int ocResolve_MachO        ( ObjectCode* oc );

static int machoGetMisalignment( FILE * );
#ifdef powerpc_HOST_ARCH
static int ocAllocateJumpIslands_MachO ( ObjectCode* oc );
static void machoInitSymbolsWithoutUnderscore( void );
#endif
#endif

#if defined(x86_64_HOST_ARCH)
static void*x86_64_high_symbol( char *lbl, void *addr );
#endif

/* -----------------------------------------------------------------------------
 * Built-in symbols from the RTS
 */

typedef struct _RtsSymbolVal {
    char   *lbl;
    void   *addr;
} RtsSymbolVal;


#if !defined(PAR)
#define Maybe_Stable_Names      SymX(mkWeakzh_fast)			\
      				SymX(makeStableNamezh_fast)		\
      				SymX(finalizzeWeakzh_fast)
#else
/* These are not available in GUM!!! -- HWL */
#define Maybe_Stable_Names
#endif

#if !defined (mingw32_HOST_OS)
#define RTS_POSIX_ONLY_SYMBOLS                  \
      SymX(signal_handlers)			\
      SymX(stg_sig_install)			\
      Sym(nocldstop)
#endif

#if defined (cygwin32_HOST_OS)
#define RTS_MINGW_ONLY_SYMBOLS /**/
/* Don't have the ability to read import libs / archives, so
 * we have to stupidly list a lot of what libcygwin.a
 * exports; sigh.
 */
#define RTS_CYGWIN_ONLY_SYMBOLS                 \
      SymX(regfree)                             \
      SymX(regexec)                             \
      SymX(regerror)                            \
      SymX(regcomp)                             \
      SymX(__errno)                             \
      SymX(access)                              \
      SymX(chmod)                               \
      SymX(chdir)                               \
      SymX(close)                               \
      SymX(creat)                               \
      SymX(dup)                                 \
      SymX(dup2)                                \
      SymX(fstat)                               \
      SymX(fcntl)                               \
      SymX(getcwd)                              \
      SymX(getenv)                              \
      SymX(lseek)                               \
      SymX(open)                                \
      SymX(fpathconf)                           \
      SymX(pathconf)                            \
      SymX(stat)                                \
      SymX(pow)                                 \
      SymX(tanh)                                \
      SymX(cosh)                                \
      SymX(sinh)                                \
      SymX(atan)                                \
      SymX(acos)                                \
      SymX(asin)                                \
      SymX(tan)                                 \
      SymX(cos)                                 \
      SymX(sin)                                 \
      SymX(exp)                                 \
      SymX(log)                                 \
      SymX(sqrt)                                \
      SymX(localtime_r)                         \
      SymX(gmtime_r)                            \
      SymX(mktime)                              \
      Sym(_imp___tzname)                        \
      SymX(gettimeofday)                        \
      SymX(timezone)                            \
      SymX(tcgetattr)                           \
      SymX(tcsetattr)                           \
      SymX(memcpy)                              \
      SymX(memmove)                             \
      SymX(realloc)                             \
      SymX(malloc)                              \
      SymX(free)                                \
      SymX(fork)                                \
      SymX(lstat)                               \
      SymX(isatty)                              \
      SymX(mkdir)                               \
      SymX(opendir)                             \
      SymX(readdir)                             \
      SymX(rewinddir)                           \
      SymX(closedir)                            \
      SymX(link)                                \
      SymX(mkfifo)                              \
      SymX(pipe)                                \
      SymX(read)                                \
      SymX(rename)                              \
      SymX(rmdir)                               \
      SymX(select)                              \
      SymX(system)                              \
      SymX(write)                               \
      SymX(strcmp)                              \
      SymX(strcpy)                              \
      SymX(strncpy)                             \
      SymX(strerror)                            \
      SymX(sigaddset)                           \
      SymX(sigemptyset)                         \
      SymX(sigprocmask)                         \
      SymX(umask)                               \
      SymX(uname)                               \
      SymX(unlink)                              \
      SymX(utime)                               \
      SymX(waitpid)

#elif !defined(mingw32_HOST_OS)
#define RTS_MINGW_ONLY_SYMBOLS /**/
#define RTS_CYGWIN_ONLY_SYMBOLS /**/
#else /* defined(mingw32_HOST_OS) */
#define RTS_POSIX_ONLY_SYMBOLS  /**/
#define RTS_CYGWIN_ONLY_SYMBOLS /**/

/* Extra syms gen'ed by mingw-2's gcc-3.2: */
#if __GNUC__>=3
#define RTS_MINGW_EXTRA_SYMS                    \
      Sym(_imp____mb_cur_max)                   \
      Sym(_imp___pctype)
#else
#define RTS_MINGW_EXTRA_SYMS
#endif

/* These are statically linked from the mingw libraries into the ghc
   executable, so we have to employ this hack. */
#define RTS_MINGW_ONLY_SYMBOLS                  \
      SymX(asyncReadzh_fast)			\
      SymX(asyncWritezh_fast)			\
      SymX(asyncDoProczh_fast)			\
      SymX(memset)                              \
      SymX(inet_ntoa)                           \
      SymX(inet_addr)                           \
      SymX(htonl)                               \
      SymX(recvfrom)                            \
      SymX(listen)                              \
      SymX(bind)                                \
      SymX(shutdown)                            \
      SymX(connect)                             \
      SymX(htons)                               \
      SymX(ntohs)                               \
      SymX(getservbyname)                       \
      SymX(getservbyport)                       \
      SymX(getprotobynumber)                    \
      SymX(getprotobyname)                      \
      SymX(gethostbyname)                       \
      SymX(gethostbyaddr)                       \
      SymX(gethostname)                         \
      SymX(strcpy)                              \
      SymX(strncpy)                             \
      SymX(abort)                               \
      Sym(_alloca)                              \
      Sym(isxdigit)                             \
      Sym(isupper)                              \
      Sym(ispunct)                              \
      Sym(islower)                              \
      Sym(isspace)                              \
      Sym(isprint)                              \
      Sym(isdigit)                              \
      Sym(iscntrl)                              \
      Sym(isalpha)                              \
      Sym(isalnum)                              \
      SymX(strcmp)                              \
      SymX(memmove)                             \
      SymX(realloc)                             \
      SymX(malloc)                              \
      SymX(pow)                                 \
      SymX(tanh)                                \
      SymX(cosh)                                \
      SymX(sinh)                                \
      SymX(atan)                                \
      SymX(acos)                                \
      SymX(asin)                                \
      SymX(tan)                                 \
      SymX(cos)                                 \
      SymX(sin)                                 \
      SymX(exp)                                 \
      SymX(log)                                 \
      SymX(sqrt)                                \
      SymX(powf)                                 \
      SymX(tanhf)                                \
      SymX(coshf)                                \
      SymX(sinhf)                                \
      SymX(atanf)                                \
      SymX(acosf)                                \
      SymX(asinf)                                \
      SymX(tanf)                                 \
      SymX(cosf)                                 \
      SymX(sinf)                                 \
      SymX(expf)                                 \
      SymX(logf)                                 \
      SymX(sqrtf)                                \
      SymX(memcpy)                              \
      SymX(rts_InstallConsoleEvent)             \
      SymX(rts_ConsoleHandlerDone)              \
      Sym(mktime)                               \
      Sym(_imp___timezone)                      \
      Sym(_imp___tzname)                        \
      Sym(_imp__tzname)                         \
      Sym(_imp___iob)                           \
      Sym(_imp___osver)                         \
      Sym(localtime)                            \
      Sym(gmtime)                               \
      Sym(opendir)                              \
      Sym(readdir)                              \
      Sym(rewinddir)                            \
      RTS_MINGW_EXTRA_SYMS                      \
      Sym(closedir)
#endif

#if defined(darwin_TARGET_OS) && HAVE_PRINTF_LDBLSTUB
#define RTS_DARWIN_ONLY_SYMBOLS			\
     Sym(asprintf$LDBLStub)                     \
     Sym(err$LDBLStub)                          \
     Sym(errc$LDBLStub)                         \
     Sym(errx$LDBLStub)                         \
     Sym(fprintf$LDBLStub)                      \
     Sym(fscanf$LDBLStub)                       \
     Sym(fwprintf$LDBLStub)                     \
     Sym(fwscanf$LDBLStub)                      \
     Sym(printf$LDBLStub)                       \
     Sym(scanf$LDBLStub)                        \
     Sym(snprintf$LDBLStub)                     \
     Sym(sprintf$LDBLStub)                      \
     Sym(sscanf$LDBLStub)                       \
     Sym(strtold$LDBLStub)                      \
     Sym(swprintf$LDBLStub)                     \
     Sym(swscanf$LDBLStub)                      \
     Sym(syslog$LDBLStub)                       \
     Sym(vasprintf$LDBLStub)                    \
     Sym(verr$LDBLStub)                         \
     Sym(verrc$LDBLStub)                        \
     Sym(verrx$LDBLStub)                        \
     Sym(vfprintf$LDBLStub)                     \
     Sym(vfscanf$LDBLStub)                      \
     Sym(vfwprintf$LDBLStub)                    \
     Sym(vfwscanf$LDBLStub)                     \
     Sym(vprintf$LDBLStub)                      \
     Sym(vscanf$LDBLStub)                       \
     Sym(vsnprintf$LDBLStub)                    \
     Sym(vsprintf$LDBLStub)                     \
     Sym(vsscanf$LDBLStub)                      \
     Sym(vswprintf$LDBLStub)                    \
     Sym(vswscanf$LDBLStub)                     \
     Sym(vsyslog$LDBLStub)                      \
     Sym(vwarn$LDBLStub)                        \
     Sym(vwarnc$LDBLStub)                       \
     Sym(vwarnx$LDBLStub)                       \
     Sym(vwprintf$LDBLStub)                     \
     Sym(vwscanf$LDBLStub)                      \
     Sym(warn$LDBLStub)                         \
     Sym(warnc$LDBLStub)                        \
     Sym(warnx$LDBLStub)                        \
     Sym(wcstold$LDBLStub)                      \
     Sym(wprintf$LDBLStub)                      \
     Sym(wscanf$LDBLStub)
#else
#define RTS_DARWIN_ONLY_SYMBOLS
#endif

#ifndef SMP
# define MAIN_CAP_SYM SymX(MainCapability)
#else
# define MAIN_CAP_SYM
#endif

#if !defined(mingw32_HOST_OS)
#define RTS_USER_SIGNALS_SYMBOLS \
   SymX(setIOManagerPipe)
#else
#define RTS_USER_SIGNALS_SYMBOLS /* nothing */
#endif

#ifdef TABLES_NEXT_TO_CODE
#define RTS_RET_SYMBOLS /* nothing */
#else
#define RTS_RET_SYMBOLS 			\
      SymX(stg_enter_ret)			\
      SymX(stg_gc_fun_ret)			\
      SymX(stg_ap_v_ret)			\
      SymX(stg_ap_f_ret)			\
      SymX(stg_ap_d_ret)			\
      SymX(stg_ap_l_ret)			\
      SymX(stg_ap_n_ret)			\
      SymX(stg_ap_p_ret)			\
      SymX(stg_ap_pv_ret)			\
      SymX(stg_ap_pp_ret)			\
      SymX(stg_ap_ppv_ret)			\
      SymX(stg_ap_ppp_ret)			\
      SymX(stg_ap_pppv_ret)			\
      SymX(stg_ap_pppp_ret)			\
      SymX(stg_ap_ppppp_ret)			\
      SymX(stg_ap_pppppp_ret)
#endif

#define RTS_SYMBOLS				\
      Maybe_Stable_Names			\
      Sym(StgReturn)				\
      SymX(stg_enter_info)			\
      SymX(stg_gc_void_info)			\
      SymX(__stg_gc_enter_1)			\
      SymX(stg_gc_noregs)			\
      SymX(stg_gc_unpt_r1_info)			\
      SymX(stg_gc_unpt_r1)			\
      SymX(stg_gc_unbx_r1_info)			\
      SymX(stg_gc_unbx_r1)			\
      SymX(stg_gc_f1_info)			\
      SymX(stg_gc_f1)				\
      SymX(stg_gc_d1_info)			\
      SymX(stg_gc_d1)				\
      SymX(stg_gc_l1_info)			\
      SymX(stg_gc_l1)				\
      SymX(__stg_gc_fun)			\
      SymX(stg_gc_fun_info)			\
      SymX(stg_gc_gen)				\
      SymX(stg_gc_gen_info)			\
      SymX(stg_gc_gen_hp)			\
      SymX(stg_gc_ut)				\
      SymX(stg_gen_yield)			\
      SymX(stg_yield_noregs)			\
      SymX(stg_yield_to_interpreter)		\
      SymX(stg_gen_block)			\
      SymX(stg_block_noregs)			\
      SymX(stg_block_1)				\
      SymX(stg_block_takemvar)			\
      SymX(stg_block_putmvar)			\
      SymX(stg_seq_frame_info)			\
      MAIN_CAP_SYM                              \
      SymX(MallocFailHook)			\
      SymX(OnExitHook)				\
      SymX(OutOfHeapHook)			\
      SymX(StackOverflowHook)			\
      SymX(__encodeDouble)			\
      SymX(__encodeFloat)			\
      SymX(addDLL)               		\
      SymX(__gmpn_gcd_1)			\
      SymX(__gmpz_cmp)				\
      SymX(__gmpz_cmp_si)			\
      SymX(__gmpz_cmp_ui)			\
      SymX(__gmpz_get_si)			\
      SymX(__gmpz_get_ui)			\
      SymX(__int_encodeDouble)			\
      SymX(__int_encodeFloat)			\
      SymX(andIntegerzh_fast)			\
      SymX(atomicallyzh_fast)			\
      SymX(barf)				\
      SymX(debugBelch)				\
      SymX(errorBelch)				\
      SymX(blockAsyncExceptionszh_fast)		\
      SymX(catchzh_fast)			\
      SymX(catchRetryzh_fast)			\
      SymX(catchSTMzh_fast)			\
      SymX(checkzh_fast)                        \
      SymX(closure_flags)                       \
      SymX(cmp_thread)				\
      SymX(cmpIntegerzh_fast)	        	\
      SymX(cmpIntegerIntzh_fast)	      	\
      SymX(complementIntegerzh_fast)		\
      SymX(createAdjustor)			\
      SymX(decodeDoublezh_fast)			\
      SymX(decodeFloatzh_fast)			\
      SymX(defaultsHook)			\
      SymX(delayzh_fast)			\
      SymX(deRefWeakzh_fast)			\
      SymX(deRefStablePtrzh_fast)		\
      SymX(dirty_MUT_VAR)			\
      SymX(divExactIntegerzh_fast)		\
      SymX(divModIntegerzh_fast)		\
      SymX(forkzh_fast)				\
      SymX(forkOnzh_fast)			\
      SymX(forkProcess)				\
      SymX(forkOS_createThread)			\
      SymX(freeHaskellFunctionPtr)		\
      SymX(freeStablePtr)		        \
      SymX(getOrSetTypeableStore)		\
      SymX(gcdIntegerzh_fast)			\
      SymX(gcdIntegerIntzh_fast)		\
      SymX(gcdIntzh_fast)			\
      SymX(genSymZh)				\
      SymX(genericRaise)			\
      SymX(getProgArgv)				\
      SymX(getStablePtr)			\
      SymX(hs_init)				\
      SymX(hs_exit)				\
      SymX(hs_set_argv)				\
      SymX(hs_add_root)				\
      SymX(hs_perform_gc)			\
      SymX(hs_free_stable_ptr)			\
      SymX(hs_free_fun_ptr)			\
      SymX(initLinker)				\
      SymX(int2Integerzh_fast)			\
      SymX(integer2Intzh_fast)			\
      SymX(integer2Wordzh_fast)			\
      SymX(isCurrentThreadBoundzh_fast)		\
      SymX(isDoubleDenormalized)		\
      SymX(isDoubleInfinite)			\
      SymX(isDoubleNaN)				\
      SymX(isDoubleNegativeZero)		\
      SymX(isEmptyMVarzh_fast)			\
      SymX(isFloatDenormalized)			\
      SymX(isFloatInfinite)			\
      SymX(isFloatNaN)				\
      SymX(isFloatNegativeZero)			\
      SymX(killThreadzh_fast)			\
      SymX(loadObj)          			\
      SymX(insertStableSymbol) 			\
      SymX(insertSymbol)     			\
      SymX(lookupSymbol)     			\
      SymX(makeStablePtrzh_fast)		\
      SymX(minusIntegerzh_fast)			\
      SymX(mkApUpd0zh_fast)			\
      SymX(myThreadIdzh_fast)			\
      SymX(labelThreadzh_fast)                  \
      SymX(newArrayzh_fast)			\
      SymX(newBCOzh_fast)			\
      SymX(newByteArrayzh_fast)			\
      SymX_redirect(newCAF, newDynCAF)		\
      SymX(newMVarzh_fast)			\
      SymX(newMutVarzh_fast)			\
      SymX(newTVarzh_fast)			\
      SymX(atomicModifyMutVarzh_fast)		\
      SymX(newPinnedByteArrayzh_fast)		\
      SymX(newSpark)				\
      SymX(orIntegerzh_fast)			\
      SymX(performGC)				\
      SymX(performMajorGC)			\
      SymX(plusIntegerzh_fast)			\
      SymX(prog_argc)				\
      SymX(prog_argv)				\
      SymX(putMVarzh_fast)			\
      SymX(quotIntegerzh_fast)			\
      SymX(quotRemIntegerzh_fast)		\
      SymX(raisezh_fast)			\
      SymX(raiseIOzh_fast)			\
      SymX(readTVarzh_fast)			\
      SymX(remIntegerzh_fast)			\
      SymX(resetNonBlockingFd)			\
      SymX(resumeThread)			\
      SymX(resolveObjs)                         \
      SymX(retryzh_fast)                        \
      SymX(rts_apply)				\
      SymX(rts_checkSchedStatus)		\
      SymX(rts_eval)				\
      SymX(rts_evalIO)				\
      SymX(rts_evalLazyIO)			\
      SymX(rts_evalStableIO)			\
      SymX(rts_eval_)				\
      SymX(rts_getBool)				\
      SymX(rts_getChar)				\
      SymX(rts_getDouble)			\
      SymX(rts_getFloat)			\
      SymX(rts_getInt)				\
      SymX(rts_getInt32)			\
      SymX(rts_getPtr)				\
      SymX(rts_getFunPtr)			\
      SymX(rts_getStablePtr)			\
      SymX(rts_getThreadId)			\
      SymX(rts_getWord)				\
      SymX(rts_getWord32)			\
      SymX(rts_lock)				\
      SymX(rts_mkBool)				\
      SymX(rts_mkChar)				\
      SymX(rts_mkDouble)			\
      SymX(rts_mkFloat)				\
      SymX(rts_mkInt)				\
      SymX(rts_mkInt16)				\
      SymX(rts_mkInt32)				\
      SymX(rts_mkInt64)				\
      SymX(rts_mkInt8)				\
      SymX(rts_mkPtr)				\
      SymX(rts_mkFunPtr)			\
      SymX(rts_mkStablePtr)			\
      SymX(rts_mkString)			\
      SymX(rts_mkWord)				\
      SymX(rts_mkWord16)			\
      SymX(rts_mkWord32)			\
      SymX(rts_mkWord64)			\
      SymX(rts_mkWord8)				\
      SymX(rts_unlock)				\
      SymX(rtsSupportsBoundThreads)		\
      SymX(__hscore_get_saved_termios)		\
      SymX(__hscore_set_saved_termios)		\
      SymX(setProgArgv)				\
      SymX(startupHaskell)			\
      SymX(shutdownHaskell)			\
      SymX(shutdownHaskellAndExit)		\
      SymX(stable_ptr_table)			\
      SymX(stackOverflow)			\
      SymX(stg_CAF_BLACKHOLE_info)		\
      SymX(awakenBlockedQueue)			\
      SymX(stg_CHARLIKE_closure)		\
      SymX(stg_EMPTY_MVAR_info)			\
      SymX(stg_IND_STATIC_info)			\
      SymX(stg_INTLIKE_closure)			\
      SymX(stg_MUT_ARR_PTRS_DIRTY_info)		\
      SymX(stg_MUT_ARR_PTRS_FROZEN_info)	\
      SymX(stg_MUT_ARR_PTRS_FROZEN0_info)	\
      SymX(stg_WEAK_info)                       \
      SymX(stg_ap_v_info)			\
      SymX(stg_ap_f_info)			\
      SymX(stg_ap_d_info)			\
      SymX(stg_ap_l_info)			\
      SymX(stg_ap_n_info)			\
      SymX(stg_ap_p_info)			\
      SymX(stg_ap_pv_info)			\
      SymX(stg_ap_pp_info)			\
      SymX(stg_ap_ppv_info)			\
      SymX(stg_ap_ppp_info)			\
      SymX(stg_ap_pppv_info)			\
      SymX(stg_ap_pppp_info)			\
      SymX(stg_ap_ppppp_info)			\
      SymX(stg_ap_pppppp_info)			\
      SymX(stg_ap_0_fast)			\
      SymX(stg_ap_v_fast)			\
      SymX(stg_ap_f_fast)			\
      SymX(stg_ap_d_fast)			\
      SymX(stg_ap_l_fast)			\
      SymX(stg_ap_n_fast)			\
      SymX(stg_ap_p_fast)			\
      SymX(stg_ap_pv_fast)			\
      SymX(stg_ap_pp_fast)			\
      SymX(stg_ap_ppv_fast)			\
      SymX(stg_ap_ppp_fast)			\
      SymX(stg_ap_pppv_fast)			\
      SymX(stg_ap_pppp_fast)			\
      SymX(stg_ap_ppppp_fast)			\
      SymX(stg_ap_pppppp_fast)			\
      SymX(stg_ap_1_upd_info)			\
      SymX(stg_ap_2_upd_info)			\
      SymX(stg_ap_3_upd_info)			\
      SymX(stg_ap_4_upd_info)			\
      SymX(stg_ap_5_upd_info)			\
      SymX(stg_ap_6_upd_info)			\
      SymX(stg_ap_7_upd_info)			\
      SymX(stg_exit)				\
      SymX(stg_sel_0_upd_info)			\
      SymX(stg_sel_10_upd_info)			\
      SymX(stg_sel_11_upd_info)			\
      SymX(stg_sel_12_upd_info)			\
      SymX(stg_sel_13_upd_info)			\
      SymX(stg_sel_14_upd_info)			\
      SymX(stg_sel_15_upd_info)			\
      SymX(stg_sel_1_upd_info)			\
      SymX(stg_sel_2_upd_info)			\
      SymX(stg_sel_3_upd_info)			\
      SymX(stg_sel_4_upd_info)			\
      SymX(stg_sel_5_upd_info)			\
      SymX(stg_sel_6_upd_info)			\
      SymX(stg_sel_7_upd_info)			\
      SymX(stg_sel_8_upd_info)			\
      SymX(stg_sel_9_upd_info)			\
      SymX(stg_upd_frame_info)			\
      SymX(suspendThread)			\
      SymX(takeMVarzh_fast)			\
      SymX(timesIntegerzh_fast)			\
      SymX(tryPutMVarzh_fast)			\
      SymX(tryTakeMVarzh_fast)			\
      SymX(unblockAsyncExceptionszh_fast)	\
      SymX(unloadObj)                           \
      SymX(unsafeThawArrayzh_fast)		\
      SymX(waitReadzh_fast)			\
      SymX(waitWritezh_fast)			\
      SymX(word2Integerzh_fast)			\
      SymX(writeTVarzh_fast)			\
      SymX(xorIntegerzh_fast)			\
      SymX(yieldzh_fast)                        \
      SymX(stg_interp_constr_entry)             \
      SymX(stg_interp_constr1_entry)            \
      SymX(stg_interp_constr2_entry)            \
      SymX(stg_interp_constr3_entry)            \
      SymX(stg_interp_constr4_entry)            \
      SymX(stg_interp_constr5_entry)            \
      SymX(stg_interp_constr6_entry)            \
      SymX(stg_interp_constr7_entry)            \
      SymX(stg_interp_constr8_entry)            \
      SymX(allocateExec)	                \
      SymX(freeExec)		                \
      SymX(getAllocations)                      \
      SymX(revertCAFs)                          \
      SymX(RtsFlags)                            \
      RTS_USER_SIGNALS_SYMBOLS

#ifdef SUPPORT_LONG_LONGS
#define RTS_LONG_LONG_SYMS			\
      SymX(int64ToIntegerzh_fast)		\
      SymX(word64ToIntegerzh_fast)
#else
#define RTS_LONG_LONG_SYMS /* nothing */
#endif

// 64-bit support functions in libgcc.a
#if defined(__GNUC__) && SIZEOF_VOID_P <= 4
#define RTS_LIBGCC_SYMBOLS			\
      Sym(__divdi3)                             \
      Sym(__udivdi3)                            \
      Sym(__moddi3)                             \
      Sym(__umoddi3)				\
      Sym(__muldi3)				\
      Sym(__ashldi3)				\
      Sym(__ashrdi3)				\
      Sym(__lshrdi3)				\
      Sym(__eprintf)
#elif defined(ia64_HOST_ARCH)
#define RTS_LIBGCC_SYMBOLS			\
      Sym(__divdi3)				\
      Sym(__udivdi3)                            \
      Sym(__moddi3)				\
      Sym(__umoddi3)				\
      Sym(__divsf3)				\
      Sym(__divdf3)
#else
#define RTS_LIBGCC_SYMBOLS
#endif

#if defined(darwin_HOST_OS) && defined(powerpc_HOST_ARCH)
      // Symbols that don't have a leading underscore
      // on Mac OS X. They have to receive special treatment,
      // see machoInitSymbolsWithoutUnderscore()
#define RTS_MACHO_NOUNDERLINE_SYMBOLS		\
      Sym(saveFP)				\
      Sym(restFP)
#endif

/* entirely bogus claims about types of these symbols */
#define Sym(vvv)  extern void vvv(void);
#define SymX(vvv) /**/
#define SymX_redirect(vvv,xxx) /**/
RTS_SYMBOLS
RTS_RET_SYMBOLS
RTS_LONG_LONG_SYMS
RTS_POSIX_ONLY_SYMBOLS
RTS_MINGW_ONLY_SYMBOLS
RTS_CYGWIN_ONLY_SYMBOLS
RTS_DARWIN_ONLY_SYMBOLS
RTS_LIBGCC_SYMBOLS
#undef Sym
#undef SymX
#undef SymX_redirect

#ifdef LEADING_UNDERSCORE
#define MAYBE_LEADING_UNDERSCORE_STR(s) ("_" s)
#else
#define MAYBE_LEADING_UNDERSCORE_STR(s) (s)
#endif

#define Sym(vvv) { MAYBE_LEADING_UNDERSCORE_STR(#vvv), \
                    (void*)(&(vvv)) },
#define SymX(vvv) Sym(vvv)

// SymX_redirect allows us to redirect references to one symbol to
// another symbol.  See newCAF/newDynCAF for an example.
#define SymX_redirect(vvv,xxx) \
    { MAYBE_LEADING_UNDERSCORE_STR(#vvv), \
      (void*)(&(xxx)) },

static RtsSymbolVal rtsSyms[] = {
      RTS_SYMBOLS
      RTS_RET_SYMBOLS
      RTS_LONG_LONG_SYMS
      RTS_POSIX_ONLY_SYMBOLS
      RTS_MINGW_ONLY_SYMBOLS
      RTS_CYGWIN_ONLY_SYMBOLS
      RTS_LIBGCC_SYMBOLS
#if defined(darwin_HOST_OS) && defined(i386_HOST_ARCH)
      // dyld stub code contains references to this,
      // but it should never be called because we treat
      // lazy pointers as nonlazy.
      { "dyld_stub_binding_helper", (void*)0xDEADBEEF },
#endif
      { 0, 0 } /* sentinel */
};


/* -----------------------------------------------------------------------------
 * Utilities for handling root pointers.
 * -------------------------------------------------------------------------- */


#define INIT_RPT_SIZE 64

STATIC_INLINE void
initFreeList(rootEntry *table, nat n, rootEntry *free)
{
  rootEntry *p;

  for (p = table + n - 1; p >= table; p--) {
    p->addr   = (P_)free;
    free = p;
  }
  root_ptr_free = table;
}

static void
initRootPtrTable(void)
{
  if (RPT_size > 0)
    return;

  RPT_size = INIT_RPT_SIZE;
  root_ptr_table = stgMallocBytes(RPT_size * sizeof(rootEntry),
                                    "initRootPtrTable");

  initFreeList(root_ptr_table,INIT_RPT_SIZE,NULL);
}


static void
enlargeRootPtrTable(void)
{
  nat old_RPT_size = RPT_size;

  // 2nd and subsequent times
  RPT_size *= 2;
  root_ptr_table =
    stgReallocBytes(root_ptr_table,
                    RPT_size * sizeof(rootEntry),
                    "enlargeRootPtrTable");

  initFreeList(root_ptr_table + old_RPT_size, old_RPT_size, NULL);
}

static void
addRootObject(void *addr)
{
  StgWord rt;
  initRootPtrTable();
  if (root_ptr_free == NULL) {
    enlargeRootPtrTable();
  }

  rt = root_ptr_free - root_ptr_table;
  root_ptr_free  = (rootEntry*)(root_ptr_free->addr);
  root_ptr_table[rt].addr = addr;
}

/* -----------------------------------------------------------------------------
 * Treat root pointers as roots for the garbage collector.
 * -------------------------------------------------------------------------- */

void
markRootPtrTable(evac_fn evac)
{
  rootEntry *p, *end_root_ptr_table;
  StgPtr q;

  end_root_ptr_table = &root_ptr_table[RPT_size];

  for (p = root_ptr_table; p < end_root_ptr_table; p++) {
    q = p->addr;

    if (q && (q < (P_)root_ptr_table || q >= (P_)end_root_ptr_table)) {
        evac((StgClosure **)p->addr);
    }
  }
}

/* -----------------------------------------------------------------------------
 * End of utilities for handling root pointers.
 * -------------------------------------------------------------------------- */


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
#endif

void
initLinker( void )
{
    RtsSymbolVal *sym;

    /* Make initLinker idempotent, so we can call it
       before evey relevant operation; that means we
       don't need to initialise the linker separately */
    if (linker_init_done == 1) { return; } else {
      linker_init_done = 1;
    }

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
#   endif
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

char *
addDLL( char *dll_name )
{
#  if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
   /* ------------------- ELF DLL loader ------------------- */
   void *hdl;
   char *errmsg;

   initLinker();

   hdl= dlopen(dll_name, RTLD_NOW | RTLD_GLOBAL);

   if (hdl == NULL) {
      /* dlopen failed; return a ptr to the error msg. */
      errmsg = dlerror();
      if (errmsg == NULL) errmsg = "addDLL: unknown error";
      return errmsg;
   } else {
      return NULL;
   }
   /*NOTREACHED*/

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
	 sprintf(buf, "%s.DRV", dll_name);	// KAA: allow loading of drivers (like winspool.drv)
	 instance = LoadLibrary(buf);
	 if (instance == NULL) {
		stgFree(buf);

	    /* LoadLibrary failed; return a ptr to the error msg. */
	    return "addDLL: unknown error";
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
#	if defined(x86_64_HOST_ARCH)
	val = dlsym(dl_prog_handle, lbl);
	if (val >= (void *)0x80000000) {
	    void *new_val;
	    new_val = x86_64_high_symbol(lbl, val);
	    IF_DEBUG(linker,debugBelch("lookupSymbol: relocating out of range symbol: %s = %p, now %p\n", lbl, val, new_val));
	    return new_val;
	} else {
	    return val;
	}
#	else
	return dlsym(dl_prog_handle, lbl);
#	endif
#       elif defined(OBJFORMAT_MACHO)
	if(NSIsSymbolNameDefined(lbl)) {
	    NSSymbol symbol = NSLookupAndBindSymbol(lbl);
	    return NSAddressOfSymbol(symbol);
	} else {
	    return NULL;
	}
#       elif defined(OBJFORMAT_PEi386)
        OpenedDLL* o_dll;
        void* sym;
        for (o_dll = opened_dlls; o_dll != NULL; o_dll = o_dll->next) {
	  /* debugBelch("look in %s for %s\n", o_dll->name, lbl); */
           if (lbl[0] == '_') {
              /* HACK: if the name has an initial underscore, try stripping
                 it off & look that up first. I've yet to verify whether there's
                 a Rule that governs whether an initial '_' *should always* be
                 stripped off when mapping from import lib name to the DLL name.
              */
              sym = GetProcAddress(o_dll->instance, (lbl+1));
              if (sym != NULL) {
		/*debugBelch("found %s in %s\n", lbl+1,o_dll->name);*/
		return sym;
	      }
           }
           sym = GetProcAddress(o_dll->instance, lbl);
           if (sym != NULL) {
	     /*debugBelch("found %s in %s\n", lbl,o_dll->name);*/
	     return sym;
	   }
        }
        return NULL;
#       else
        ASSERT(2+2 == 5);
        return NULL;
#       endif
    } else {
	return val;
    }
}

static
__attribute((unused))
void *
lookupLocalSymbol( ObjectCode* oc, char *lbl )
{
    void *val;
    initLinker() ;
    val = lookupStrHashTable(oc->lochash, lbl);

    if (val == NULL) {
        return NULL;
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
         // debugBelch("enquire %p %p\n", sym, oc->lochash);
         a = NULL;
         if (oc->lochash != NULL) {
            a = lookupStrHashTable(oc->lochash, sym);
	 }
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

#ifdef ia64_HOST_ARCH
static unsigned int PLTSize(void);
#endif

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
   int r, n;
#ifdef USE_MMAP
   int fd, pagesize;
   void *map_addr = NULL;
#else
   FILE *f;
   int misalignment;
#endif
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
   if (r == -1) { return 0; }

   /* sigh, strdup() isn't a POSIX function, so do it the long way */
   oc->fileName = stgMallocBytes( strlen(path)+1, "loadObj" );
   strcpy(oc->fileName, path);

   oc->fileSize          = st.st_size;
   oc->symbols           = NULL;
   oc->sections          = NULL;
   oc->lochash           = allocStrHashTable();
   oc->proddables        = NULL;

   /* chain it onto the list of objects */
   oc->next              = objects;
   objects               = oc;

#ifdef USE_MMAP
#define ROUND_UP(x,size) ((x + size - 1) & ~(size - 1))

   /* On many architectures malloc'd memory isn't executable, so we need to use mmap. */

#if defined(openbsd_HOST_OS)
   fd = open(path, O_RDONLY, S_IRUSR);
#else
   fd = open(path, O_RDONLY);
#endif
   if (fd == -1)
      barf("loadObj: can't open `%s'", path);

   pagesize = getpagesize();

#ifdef ia64_HOST_ARCH
   /* The PLT needs to be right before the object */
   n = ROUND_UP(PLTSize(), pagesize);
   oc->plt = mmap(NULL, n, PROT_EXEC|PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
   if (oc->plt == MAP_FAILED)
      barf("loadObj: can't allocate PLT");

   oc->pltIndex = 0;
   map_addr = oc->plt + n;
#endif

   n = ROUND_UP(oc->fileSize, pagesize);

   /* Link objects into the lower 2Gb on x86_64.  GHC assumes the
    * small memory model on this architecture (see gcc docs,
    * -mcmodel=small).
    */
#ifdef x86_64_HOST_ARCH
#define EXTRA_MAP_FLAGS MAP_32BIT
#else
#define EXTRA_MAP_FLAGS 0
#endif

   oc->image = mmap(map_addr, n, PROT_EXEC|PROT_READ|PROT_WRITE, 
		    MAP_PRIVATE|EXTRA_MAP_FLAGS, fd, 0);
   if (oc->image == MAP_FAILED)
      barf("loadObj: can't map `%s'", path);

   close(fd);

#else /* !USE_MMAP */

   /* load the image into memory */
   f = fopen(path, "rb");
   if (!f)
       barf("loadObj: can't read `%s'", path);

#ifdef darwin_HOST_OS
    // In a Mach-O .o file, all sections can and will be misaligned
    // if the total size of the headers is not a multiple of the
    // desired alignment. This is fine for .o files that only serve
    // as input for the static linker, but it's not fine for us,
    // as SSE (used by gcc for floating point) and Altivec require
    // 16-byte alignment.
    // We calculate the correct alignment from the header before
    // reading the file, and then we misalign oc->image on purpose so
    // that the actual sections end up aligned again.
   misalignment = machoGetMisalignment(f);
   oc->misalignment = misalignment;
#else
   misalignment = 0;
#endif

   oc->image = stgMallocBytes(oc->fileSize + misalignment, "loadObj(image)");
   oc->image += misalignment;
   
   n = fread ( oc->image, 1, oc->fileSize, f );
   if (n != oc->fileSize)
      barf("loadObj: error whilst reading `%s'", path);

   fclose(f);

#endif /* USE_MMAP */

#  if defined(OBJFORMAT_MACHO) && defined(powerpc_HOST_ARCH)
   r = ocAllocateJumpIslands_MachO ( oc );
   if (!r) { return r; }
#  elif defined(OBJFORMAT_ELF) && defined(powerpc_HOST_ARCH)
   r = ocAllocateJumpIslands_ELF ( oc );
   if (!r) { return r; }
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
   if (!r) { return r; }

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
   if (!r) { return r; }

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

	    /* We're going to leave this in place, in case there are
	       any pointers from the heap into it: */
	    /* stgFree(oc->image); */
	    stgFree(oc->fileName);
	    stgFree(oc->symbols);
	    stgFree(oc->sections);
	    /* The local hash table should have been freed at the end
               of the ocResolve_ call on it. */
            ASSERT(oc->lochash == NULL);
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
 * PowerPC specifics (jump islands)
 * ------------------------------------------------------------------------*/

#if defined(powerpc_HOST_ARCH)

/*
  ocAllocateJumpIslands
  
  Allocate additional space at the end of the object file image to make room
  for jump islands.
  
  PowerPC relative branch instructions have a 24 bit displacement field.
  As PPC code is always 4-byte-aligned, this yields a +-32MB range.
  If a particular imported symbol is outside this range, we have to redirect
  the jump to a short piece of new code that just loads the 32bit absolute
  address and jumps there.
  This function just allocates space for one 16 byte ppcJumpIsland for every
  undefined symbol in the object file. The code for the islands is filled in by
  makeJumpIsland below.
*/

static int ocAllocateJumpIslands( ObjectCode* oc, int count, int first )
{
#ifdef USE_MMAP
  int pagesize, n, m;
#endif
  int aligned;
  int misalignment = 0;
#if darwin_HOST_OS
  misalignment = oc->misalignment;
#endif

  if( count > 0 )
  {
    // round up to the nearest 4
    aligned = (oc->fileSize + 3) & ~3;

#ifdef USE_MMAP
    #ifndef linux_HOST_OS /* mremap is a linux extension */
        #error ocAllocateJumpIslands doesnt want USE_MMAP to be defined
    #endif

    pagesize = getpagesize();
    n = ROUND_UP( oc->fileSize, pagesize );
    m = ROUND_UP( aligned + sizeof (ppcJumpIsland) * count, pagesize );

    /* If we have a half-page-size file and map one page of it then
     * the part of the page after the size of the file remains accessible.
     * If, however, we map in 2 pages, the 2nd page is not accessible
     * and will give a "Bus Error" on access.  To get around this, we check
     * if we need any extra pages for the jump islands and map them in
     * anonymously.  We must check that we actually require extra pages
     * otherwise the attempt to mmap 0 pages of anonymous memory will
     * fail -EINVAL.
     */

    if( m > n )
    {
      /* The effect of this mremap() call is only the ensure that we have
       * a sufficient number of virtually contiguous pages.  As returned from
       * mremap, the pages past the end of the file are not backed.  We give
       * them a backing by using MAP_FIXED to map in anonymous pages.
       */
      oc->image = mremap( oc->image, n, m, MREMAP_MAYMOVE );

      if( oc->image == MAP_FAILED )
      {
        errorBelch( "Unable to mremap for Jump Islands\n" );
        return 0;
      }

      if( mmap( oc->image + n, m - n, PROT_READ | PROT_WRITE | PROT_EXEC,
                MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, 0, 0 ) == MAP_FAILED )
      {
        errorBelch( "Unable to mmap( MAP_FIXED ) for Jump Islands\n" );
        return 0;
      }
    }

#else
    oc->image -= misalignment;
    oc->image = stgReallocBytes( oc->image,
                                 misalignment + 
                                 aligned + sizeof (ppcJumpIsland) * count,
                                 "ocAllocateJumpIslands" );
    oc->image += misalignment;
#endif /* USE_MMAP */

    oc->jump_islands = (ppcJumpIsland *) (oc->image + aligned);
    memset( oc->jump_islands, 0, sizeof (ppcJumpIsland) * count );
  }
  else
    oc->jump_islands = NULL;

  oc->island_start_symbol = first;
  oc->n_islands = count;

  return 1;
}

static unsigned long makeJumpIsland( ObjectCode* oc,
                                     unsigned long symbolNumber,
                                     unsigned long target )
{
  ppcJumpIsland *island;

  if( symbolNumber < oc->island_start_symbol ||
      symbolNumber - oc->island_start_symbol > oc->n_islands)
    return 0;

  island = &oc->jump_islands[symbolNumber - oc->island_start_symbol];

  // lis r12, hi16(target)
  island->lis_r12     = 0x3d80;
  island->hi_addr     = target >> 16;

  // ori r12, r12, lo16(target)
  island->ori_r12_r12 = 0x618c;
  island->lo_addr     = target & 0xffff;

  // mtctr r12
  island->mtctr_r12   = 0x7d8903a6;

  // bctr
  island->bctr        = 0x4e800420;
    
  return (unsigned long) island;
}

/*
   ocFlushInstructionCache

   Flush the data & instruction caches.
   Because the PPC has split data/instruction caches, we have to
   do that whenever we modify code at runtime.
 */

static void ocFlushInstructionCache( ObjectCode *oc )
{
    int n = (oc->fileSize + sizeof( ppcJumpIsland ) * oc->n_islands + 3) / 4;
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
      strncpy ( dst, strtab+strtab_offset, dstSize );
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
   strncpy(newstr,name,8);
   newstr[8] = 0;
   return newstr;
}


/* Just compares the short names (first 8 chars) */
static COFF_section *
findPEi386SectionCalled ( ObjectCode* oc,  char* name )
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
      if (0 != strcmp(sectab_i->Name, ".bss")) continue;
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

      if (0==strcmp(".text",sectab_i->Name) ||
          0==strcmp(".rdata",sectab_i->Name)||
          0==strcmp(".rodata",sectab_i->Name))
         kind = SECTIONKIND_CODE_OR_RODATA;
      if (0==strcmp(".data",sectab_i->Name) ||
          0==strcmp(".bss",sectab_i->Name))
         kind = SECTIONKIND_RWDATA;

      ASSERT(sectab_i->SizeOfRawData == 0 || sectab_i->VirtualSize == 0);
      sz = sectab_i->SizeOfRawData;
      if (sz < sectab_i->VirtualSize) sz = sectab_i->VirtualSize;

      start = ((UChar*)(oc->image)) + sectab_i->PointerToRawData;
      end   = start + sz - 1;

      if (kind == SECTIONKIND_OTHER
          /* Ignore sections called which contain stabs debugging
             information. */
          && 0 != strcmp(".stab", sectab_i->Name)
          && 0 != strcmp(".stabstr", sectab_i->Name)
          /* ignore constructor section for now */
          && 0 != strcmp(".ctors", sectab_i->Name)
          /* ignore section generated from .ident */
          && 0!= strcmp("/4", sectab_i->Name)
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
         oc->symbols[i] = sname;
         ghciInsertStrHashTable(oc->fileName, symhash, sname, addr);
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
   char symbol[1000];
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
      if (0 == strcmp(".stab", sectab_i->Name)
          || 0 == strcmp(".stabstr", sectab_i->Name)
          || 0 == strcmp(".ctors", sectab_i->Name))
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
            S = (UInt32) lookupLocalSymbol( oc, symbol );
            if ((void*)S != NULL) goto foundit;
            S = (UInt32) lookupSymbol( symbol );
            if ((void*)S != NULL) goto foundit;
            zapTrailingAtSign ( symbol );
            S = (UInt32) lookupLocalSymbol( oc, symbol );
            if ((void*)S != NULL) goto foundit;
            S = (UInt32) lookupSymbol( symbol );
            if ((void*)S != NULL) goto foundit;
	    /* Newline first because the interactive linker has printed "linking..." */
            errorBelch("\n%s: unknown symbol `%s'", oc->fileName, symbol);
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
#elif defined (ia64_HOST_ARCH)
#  define ELF_TARGET_IA64   /* Used inside <elf.h> */
#  define ELF_64BIT
#  define ELF_FUNCTION_DESC /* calling convention uses function descriptors */
#  define ELF_NEED_GOT      /* needs Global Offset Table */
#  define ELF_NEED_PLT      /* needs Procedure Linkage Tables */
#endif

#if !defined(openbsd_HOST_OS)
#include <elf.h>
#else
/* openbsd elf has things in different places, with diff names */
#include <elf_abi.h>
#include <machine/reloc.h>
#define R_386_32    RELOC_32
#define R_386_PC32  RELOC_PC32
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
#define ELF_ST_TYPE ELF64_ST_TYPE
#define ELF_ST_BIND ELF64_ST_BIND
#define ELF_R_TYPE  ELF64_R_TYPE
#define ELF_R_SYM   ELF64_R_SYM
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
#ifdef ia64_HOST_ARCH
static void ia64_reloc_gprel22(Elf_Addr target, Elf_Addr value);
static void ia64_reloc_pcrel21(Elf_Addr target, Elf_Addr value, ObjectCode *oc);

static unsigned char plt_code[] =
{
   /* taken from binutils bfd/elfxx-ia64.c */
   0x0b, 0x78, 0x00, 0x02, 0x00, 0x24,  /*   [MMI]       addl r15=0,r1;;    */
   0x00, 0x41, 0x3c, 0x30, 0x28, 0xc0,  /*               ld8 r16=[r15],8    */
   0x01, 0x08, 0x00, 0x84,              /*               mov r14=r1;;       */
   0x11, 0x08, 0x00, 0x1e, 0x18, 0x10,  /*   [MIB]       ld8 r1=[r15]       */
   0x60, 0x80, 0x04, 0x80, 0x03, 0x00,  /*               mov b6=r16         */
   0x60, 0x00, 0x80, 0x00               /*               br.few b6;;        */
};

/* If we can't get to the function descriptor via gp, take a local copy of it */
#define PLT_RELOC(code, target) { \
   Elf64_Sxword rel_value = target - gp_val; \
   if ((rel_value > 0x1fffff) || (rel_value < -0x1fffff)) \
      ia64_reloc_gprel22((Elf_Addr)code, copyFunctionDesc(target)); \
   else \
      ia64_reloc_gprel22((Elf_Addr)code, target); \
   }
#endif

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


#if x86_64_HOST_ARCH
// On x86_64, 32-bit relocations are often used, which requires that
// we can resolve a symbol to a 32-bit offset.  However, shared
// libraries are placed outside the 2Gb area, which leaves us with a
// problem when we need to give a 32-bit offset to a symbol in a
// shared library.
// 
// For a function symbol, we can allocate a bounce sequence inside the
// 2Gb area and resolve the symbol to this.  The bounce sequence is
// simply a long jump instruction to the real location of the symbol.
//
// For data references, we're screwed.
//
typedef struct {
    unsigned char jmp[8];  /* 6 byte instruction: jmpq *0x00000002(%rip) */
    void *addr;
} x86_64_bounce;

#define X86_64_BB_SIZE 1024

static x86_64_bounce *x86_64_bounce_buffer = NULL;
static nat x86_64_bb_next_off;

static void*
x86_64_high_symbol( char *lbl, void *addr )
{
    x86_64_bounce *bounce;

    if ( x86_64_bounce_buffer == NULL || 
	 x86_64_bb_next_off >= X86_64_BB_SIZE ) {
	x86_64_bounce_buffer = 
	    mmap(NULL, X86_64_BB_SIZE * sizeof(x86_64_bounce), 
		 PROT_EXEC|PROT_READ|PROT_WRITE, 
		 MAP_PRIVATE|MAP_32BIT|MAP_ANONYMOUS, -1, 0);
	if (x86_64_bounce_buffer == MAP_FAILED) {
	    barf("x86_64_high_symbol: mmap failed");
	}
	x86_64_bb_next_off = 0;
    }
    bounce = &x86_64_bounce_buffer[x86_64_bb_next_off];
    bounce->jmp[0] = 0xff;
    bounce->jmp[1] = 0x25;
    bounce->jmp[2] = 0x02;
    bounce->jmp[3] = 0x00;
    bounce->jmp[4] = 0x00;
    bounce->jmp[5] = 0x00;
    bounce->addr = addr;
    x86_64_bb_next_off++;

    IF_DEBUG(linker, debugBelch("x86_64: allocated bounce entry for %s->%p at %p\n",
				lbl, addr, bounce));

    insertStrHashTable(symhash, lbl, bounce);
    return bounce;
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

#if defined(ia64_HOST_ARCH)
static Elf_Addr
findElfSegment ( void* objImage, Elf_Addr vaddr )
{
   char* ehdrC = (char*)objImage;
   Elf_Ehdr* ehdr = (Elf_Ehdr*)ehdrC;
   Elf_Phdr* phdr = (Elf_Phdr*)(ehdrC + ehdr->e_phoff);
   Elf_Addr segaddr = 0;
   int i;

   for (i = 0; i < ehdr->e_phnum; i++) {
      segaddr = phdr[i].p_vaddr;
      if ((vaddr >= segaddr) && (vaddr < segaddr + phdr[i].p_memsz))
	      break;
   }
   return segaddr;
}
#endif

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
#endif
      default:       IF_DEBUG(linker,debugBelch( "unknown" ));
                     errorBelch("%s: unknown architecture", oc->fileName);
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
               IF_DEBUG(linker,debugBelch( "addOTabName(GLOB): %10p  %s %s",
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
              addRootObject((void*)P);
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
#     elif defined(ia64_HOST_ARCH)
      Elf64_Xword *pP = (Elf64_Xword *)P;
      Elf_Addr addr;
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

            (JRS: which means that freeloading on the R_SPARC_32 case
            is probably wrong, but hey ...)
         */
         case R_SPARC_UA32:
         case R_SPARC_32:
            w2 = (Elf_Word)value;
            *pP = w2;
            break;
#        elif defined(ia64_HOST_ARCH)
	 case R_IA64_DIR64LSB:
	 case R_IA64_FPTR64LSB:
	    *pP = value;
	    break;
	 case R_IA64_PCREL64LSB:
	    *pP = value - P;
	    break;
	 case R_IA64_SEGREL64LSB:
	    addr = findElfSegment(ehdrC, value);
	    *pP = value - addr;
	    break;
	 case R_IA64_GPREL22:
	    ia64_reloc_gprel22(P, value);
	    break;
	 case R_IA64_LTOFF22:
	 case R_IA64_LTOFF22X:
	 case R_IA64_LTOFF_FPTR22:
	    addr = allocateGOTEntry(value);
	    ia64_reloc_gprel22(P, addr);
	    break;
	 case R_IA64_PCREL21B:
	    ia64_reloc_pcrel21(P, S, oc);
	    break;
	 case R_IA64_LDXMOV:
	    /* This goes with R_IA64_LTOFF22X and points to the load to
	     * convert into a move.  We don't implement relaxation. */
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
               value = makeJumpIsland( oc, ELF_R_SYM(info), value );
               delta = value - P;

               if( value == 0 || delta << 6 >> 6 != delta )
               {
                  barf( "Unable to make ppcJumpIsland for #%d",
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
	      barf("R_X86_64_PC32 relocation out of range: %s = %p",
		   symbol, off);
	  }
	  *(Elf64_Word *)P = (Elf64_Word)off;
	  break;
      }

      case R_X86_64_32:
	  if (value >= 0x7fffffffL) {
	      barf("R_X86_64_32 relocation out of range: %s = %p\n",
		   symbol, value);
	  }
	  *(Elf64_Word *)P = (Elf64_Word)value;
	  break;

      case R_X86_64_32S:
	  if ((StgInt64)value > 0x7fffffffL || (StgInt64)value < -0x80000000L) {
	      barf("R_X86_64_32S relocation out of range: %s = %p\n",
		   symbol, value);
	  }
	  *(Elf64_Sword *)P = (Elf64_Sword)value;
	  break;
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

   /* Free the local symbol table; we won't need it again. */
   freeHashTable(oc->lochash, NULL);
   oc->lochash = NULL;

#if defined(powerpc_HOST_ARCH)
   ocFlushInstructionCache( oc );
#endif

   return 1;
}

/*
 * IA64 specifics
 * Instructions are 41 bits long, packed into 128 bit bundles with a 5-bit template
 * at the front.  The following utility functions pack and unpack instructions, and
 * take care of the most common relocations.
 */

#ifdef ia64_HOST_ARCH

static Elf64_Xword
ia64_extract_instruction(Elf64_Xword *target)
{
   Elf64_Xword w1, w2;
   int slot = (Elf_Addr)target & 3;
   target = (Elf_Addr)target & ~3;

   w1 = *target;
   w2 = *(target+1);

   switch (slot)
   {
      case 0:
         return ((w1 >> 5) & 0x1ffffffffff);
      case 1:
         return (w1 >> 46) | ((w2 & 0x7fffff) << 18);
      case 2:
         return (w2 >> 23);
      default:
         barf("ia64_extract_instruction: invalid slot %p", target);
   }
}

static void
ia64_deposit_instruction(Elf64_Xword *target, Elf64_Xword value)
{
   int slot = (Elf_Addr)target & 3;
   target = (Elf_Addr)target & ~3;

   switch (slot)
   {
      case 0:
         *target |= value << 5;
         break;
      case 1:
         *target |= value << 46;
         *(target+1) |= value >> 18;
         break;
      case 2:
         *(target+1) |= value << 23;
         break;
   }
}

static void
ia64_reloc_gprel22(Elf_Addr target, Elf_Addr value)
{
   Elf64_Xword instruction;
   Elf64_Sxword rel_value;

   rel_value = value - gp_val;
   if ((rel_value > 0x1fffff) || (rel_value < -0x1fffff))
      barf("GP-relative data out of range (address = 0x%lx, gp = 0x%lx)", value, gp_val);

   instruction = ia64_extract_instruction((Elf64_Xword *)target);
   instruction |= (((rel_value >> 0) & 0x07f) << 13)		/* imm7b */
		    | (((rel_value >> 7) & 0x1ff) << 27)	/* imm9d */
		    | (((rel_value >> 16) & 0x01f) << 22)	/* imm5c */
		    | ((Elf64_Xword)(rel_value < 0) << 36);	/* s */
   ia64_deposit_instruction((Elf64_Xword *)target, instruction);
}

static void
ia64_reloc_pcrel21(Elf_Addr target, Elf_Addr value, ObjectCode *oc)
{
   Elf64_Xword instruction;
   Elf64_Sxword rel_value;
   Elf_Addr entry;

   entry = allocatePLTEntry(value, oc);

   rel_value = (entry >> 4) - (target >> 4);
   if ((rel_value > 0xfffff) || (rel_value < -0xfffff))
      barf("PLT entry too far away (entry = 0x%lx, target = 0x%lx)", entry, target);

   instruction = ia64_extract_instruction((Elf64_Xword *)target);
   instruction |= ((rel_value & 0xfffff) << 13) 		/* imm20b */
	    	    | ((Elf64_Xword)(rel_value < 0) << 36);	/* s */
   ia64_deposit_instruction((Elf64_Xword *)target, instruction);
}

#endif /* ia64 */

/*
 * PowerPC ELF specifics
 */

#ifdef powerpc_HOST_ARCH

static int ocAllocateJumpIslands_ELF( ObjectCode *oc )
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
      shdr[i].sh_entsize, sizeof( Elf_Sym ) );
    
    return 0;
  }

  return ocAllocateJumpIslands( oc, shdr[i].sh_size / sizeof( Elf_Sym ), 0 );
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

#ifdef powerpc_HOST_ARCH
static int ocAllocateJumpIslands_MachO(ObjectCode* oc)
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
                return ocAllocateJumpIslands(oc, max - min + 1, min);

            break;
        }
        
        lc = (struct load_command *) ( ((char *)lc) + lc->cmdsize );
    }
    return ocAllocateJumpIslands(oc,0,0);
}
#endif

static int ocVerifyImage_MachO(ObjectCode* oc STG_UNUSED)
{
    // FIXME: do some verifying here
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
	else if((addr = lookupLocalSymbol(oc,nm)) != NULL)
	    ;
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
		        || scat->r_type == PPC_RELOC_HA16_SECTDIFF)
#else
                    else if(scat->r_type == GENERIC_RELOC_SECTDIFF)
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
		        continue;  // ignore the others

#ifdef powerpc_HOST_ARCH
                    if(scat->r_type == GENERIC_RELOC_VANILLA
                        || scat->r_type == PPC_RELOC_SECTDIFF)
#else
                    if(scat->r_type == GENERIC_RELOC_VANILLA
                        || scat->r_type == GENERIC_RELOC_SECTDIFF)
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
	    }

	    continue; // FIXME: I hope it's OK to ignore all the others.
	}
	else
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
                        ASSERT(-word == reloc->r_address);
                        jumpIsland = makeJumpIsland(oc,reloc->r_symbolnum,(unsigned long) symbolAddress);
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
	    barf("\nunknown relocation %d",reloc->r_type);
	    return 0;
	}
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
	if(lc->cmd == LC_SEGMENT)
	    segLC = (struct segment_command*) lc;
	else if(lc->cmd == LC_SYMTAB)
	    symLC = (struct symtab_command*) lc;
	lc = (struct load_command *) ( ((char*)lc) + lc->cmdsize );
    }

    sections = (struct section*) (segLC+1);
    nlist = symLC ? (struct nlist*) (image + symLC->symoff)
                  : NULL;

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
                    ghciInsertStrHashTable(oc->fileName, symhash, nm,
                                            image
                                            + sections[nlist[i].n_sect-1].offset
                                            - sections[nlist[i].n_sect-1].addr
                                            + nlist[i].n_value);
                    oc->symbols[curSymbol++] = nm;
                }
                else
                {
                    char *nm = image + symLC->stroff + nlist[i].n_un.n_strx;
                    ghciInsertStrHashTable(oc->fileName, oc->lochash, nm,
                                            image
                                            + sections[nlist[i].n_sect-1].offset
                                            - sections[nlist[i].n_sect-1].addr
                                            + nlist[i].n_value);
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
	if(lc->cmd == LC_SEGMENT)
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

    /* Free the local symbol table; we won't need it again. */
    freeHashTable(oc->lochash, NULL);
    oc->lochash = NULL;

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

static void machoInitSymbolsWithoutUnderscore()
{
    extern void* symbolsWithoutUnderscore[];
    void **p = symbolsWithoutUnderscore;
    __asm__ volatile(".globl _symbolsWithoutUnderscore\n.data\n_symbolsWithoutUnderscore:");

#undef Sym
#define Sym(x)  \
    __asm__ volatile(".long " # x);

    RTS_MACHO_NOUNDERLINE_SYMBOLS

    __asm__ volatile(".text");
    
#undef Sym
#define Sym(x)  \
    ghciInsertStrHashTable("(GHCi built-in symbols)", symhash, #x, *p++);
    
    RTS_MACHO_NOUNDERLINE_SYMBOLS
    
#undef Sym
}
#endif

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

    if(header.magic != MH_MAGIC)
        return 0;
    
    misalignment = (header.sizeofcmds + sizeof(header))
                    & 0xF;

    return misalignment ? (16 - misalignment) : 0;
}

#endif
