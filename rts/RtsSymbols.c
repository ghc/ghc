/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2015
 *
 * RTS Symbols
 *
 * ---------------------------------------------------------------------------*/

#include "ghcplatform.h"
#include "Rts.h"
#include "RtsSymbols.h"

#include "TopHandler.h"
#include "HsFFI.h"
#include "CloneStack.h"

#include "sm/Storage.h"
#include "sm/NonMovingMark.h"
#include "Arena.h"
#include <stdbool.h>

#if !defined(mingw32_HOST_OS) && defined(HAVE_SIGNAL_H)
#include "posix/Signals.h"
#endif

#if defined(mingw32_HOST_OS)
#include <sys/stat.h>
#include <io.h>
#include <windows.h>
#include <shfolder.h> /* SHGetFolderPathW */
#include "win32/AsyncWinIO.h"
#endif

#if defined(openbsd_HOST_OS)
#include <elf.h> /* _DYNAMIC */
#endif

#if defined(HAVE_UNISTD_H)
#include <unistd.h> /* environ */
#endif

#if !HAVE_DECL_ENVIRON
/* We must provide a prototype for environ since depending upon the libc
 * version it may or may not be provided by unistd.h. See #20577 and #20861.
 */
extern char **environ;
#endif


/* -----------------------------------------------------------------------------
 * Symbols to be inserted into the RTS symbol table.
 */

#define Maybe_Stable_Names      SymI_HasProto(stg_mkWeakzh)                   \
                                SymI_HasProto(stg_mkWeakNoFinalizzerzh)       \
                                SymI_HasProto(stg_addCFinalizzerToWeakzh)     \
                                SymI_HasProto(stg_makeStableNamezh)           \
                                SymI_HasProto(stg_finalizzeWeakzh)

#define RTS_LIBDW_SYMBOLS                       \
      SymE_HasProto(backtraceFree)              \
      SymE_HasProto(libdwGetBacktrace)          \
      SymE_HasProto(libdwLookupLocation)        \
      SymE_HasProto(libdwPoolTake)              \
      SymE_HasProto(libdwPoolRelease)           \
      SymE_HasProto(libdwPoolClear)

#if !defined(mingw32_HOST_OS) && !defined(wasm32_HOST_ARCH)
#define RTS_POSIX_ONLY_SYMBOLS                  \
      SymI_HasProto(__hscore_get_saved_termios) \
      SymI_HasProto(__hscore_set_saved_termios) \
      SymI_HasProto(shutdownHaskellAndSignal)   \
      SymI_HasProto(signal_handlers)            \
      SymI_HasProto(stg_sig_install)            \
      SymI_HasProto(rtsTimerSignal)             \
      SymI_NeedsDataProto(nocldstop)
#endif

#if defined(wasm32_HOST_ARCH)
#define RTS_POSIX_ONLY_SYMBOLS
#endif

#if defined(mingw32_HOST_OS)
#define RTS_POSIX_ONLY_SYMBOLS  /**/

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

/*
 * Note [Strong symbols]
 * ~~~~~~~~~~~~~~~~~~~~~
 * The notion of a *weak* symbol is fairly common in linking: a symbol is weak
 * if it is declared but not defined, allowing it to be defined by an object
 * which is loaded later. GHC generalizes this notion, allowing symbol
 * definitions to be declared as *strong*. A strong symbol is one which will
 * silently supersede definitions of the same name by later objects.
 *
 * This is currently only used in the case of atexit() to workaround an
 * unfortunate interaction on musl systems (#20350). Specifically,
 * we include atexit() in RtsSymbols to ensure that it can be used by foreign
 * code loaded by the RTS linker (see #4456). However, this causes trouble on
 * statically-linked musl systems since musl's libc.a defines atexit() as a
 * non-weak symbol, causing it to conflict with the symbol table entry produced
 * by the RtsSymbols entry. To avoid this we introduce a horrible special case
 * in `ghciInsertSymbolTable`, ensure that `atexit` is never overridden.
 */
/*
 * Note [Symbols for MinGW's printf]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * The printf offered by Microsoft's libc implementation, msvcrt, is quite
 * incomplete, lacking support for even %ull. Consequently mingw-w64 offers its
 * own implementation which we enable. However, to be thread-safe the
 * implementation uses _lock_file. This would be fine except msvcrt.dll doesn't
 * export _lock_file, only numbered versions do (e.g. msvcrt90.dll).
 *
 * To work around this mingw-w64 packages a static archive of msvcrt which
 * includes their own implementation of _lock_file. However, this means that
 * the archive contains things which the dynamic library does not; consequently
 * we need to ensure that the runtime linker provides this symbol.
 *
 * It's all just so terrible.
 *
 * See also:
 * https://sourceforge.net/p/mingw-w64/wiki2/gnu%20printf/
 * https://sourceforge.net/p/mingw-w64/discussion/723797/thread/55520785/
 */
/* Note [_iob_func symbol]
 * ~~~~~~~~~~~~~~~~~~~~~~~
 * Microsoft in VS2013 to VS2015 transition made a backwards incompatible change
 * to the stdio function __iob_func.
 *
 * They used to be defined as:
 *
 * #define stdin  (&__iob_func()[0])
 * #define stdout (&__iob_func()[1])
 * #define stderr (&__iob_func()[2])
 *
 * whereas now they're defined as:
 *
 * #define stdin  (__acrt_iob_func(0))
 * #define stdout (__acrt_iob_func(1))
 * #define stderr (__acrt_iob_func(2))
 *
 * Mingw-w64 followed along with the madness and so we have to deal with both
 * version of these symbols.
 *
 * As such when you mix new and old libraries you get a missing symbols error
 * for __acrt_iob_func.  It then links against the PLT for the function but that
 * no longer exists.  Instead we forward the request for the PLT symbol to the
 * symbol directly which is defined inline since we're using a newer compiler.
 *
 * See also:
 * https://docs.microsoft.com/en-us/cpp/porting/visual-cpp-change-history-2003-2015?view=vs-2017#stdioh-and-conioh
 */
#define RTS_MINGW_ONLY_SYMBOLS                           \
      SymI_HasProto(stg_asyncReadzh)                     \
      SymI_HasProto(stg_asyncWritezh)                    \
      SymI_HasProto(stg_asyncDoProczh)                   \
      SymI_HasProto(rts_InstallConsoleEvent)             \
      SymI_HasProto(rts_ConsoleHandlerDone)              \
      SymI_NeedsProto(__mingw_module_is_dll)             \
      RTS_WIN32_ONLY(SymI_NeedsProto(___chkstk_ms))      \
      RTS_WIN64_ONLY(SymI_NeedsProto(___chkstk_ms))      \
      RTS_WIN64_ONLY(SymI_HasProto(__stdio_common_vswprintf_s)) \
      RTS_WIN64_ONLY(SymI_HasProto(__stdio_common_vswprintf)) \
      RTS_WIN64_ONLY(SymI_HasProto(_errno))  \
      /* see Note [Symbols for MinGW's printf] */        \
      SymI_HasProto(_lock_file)                          \
      SymI_HasProto(_unlock_file)                        \
      /* See Note [_iob_func symbol] */                  \
      RTS_WIN64_ONLY(SymI_HasProto_redirect(             \
         __imp___acrt_iob_func, __rts_iob_func, STRENGTH_WEAK, SYM_TYPE_INDIRECT_DATA))   \
      RTS_WIN32_ONLY(SymI_HasProto_redirect(             \
         __imp____acrt_iob_func, __rts_iob_func, STRENGTH_WEAK, SYM_TYPE_INDIRECT_DATA))
#else
#define RTS_MINGW_ONLY_SYMBOLS /**/
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

#if defined(openbsd_HOST_OS)
#define RTS_OPENBSD_ONLY_SYMBOLS                            \
     SymE_NeedsProto(__guard_local)                         \
     SymE_HasProto(_DYNAMIC)
#else
#define RTS_OPENBSD_ONLY_SYMBOLS
#endif

#if !defined(SMP)
# define MAIN_CAP_SYM SymI_HasProto(MainCapability)
#else
# define MAIN_CAP_SYM
#endif

#if !defined(mingw32_HOST_OS)
#define RTS_USER_SIGNALS_SYMBOLS        \
   SymI_HasProto(setIOManagerControlFd) \
   SymI_HasProto(setTimerManagerControlFd) \
   SymI_HasProto(setIOManagerWakeupFd)  \
   SymI_HasProto(blockUserSignals)      \
   SymI_HasProto(unblockUserSignals)
#else
#define RTS_USER_SIGNALS_SYMBOLS             \
   SymI_HasProto(registerIOCPHandle)         \
   SymI_HasProto(getOverlappedEntries)       \
   SymI_HasProto(completeSynchronousRequest) \
   SymI_HasProto(registerAlertableWait)      \
   SymI_HasProto(sendIOManagerEvent)         \
   SymI_HasProto(readIOManagerEvent)         \
   SymI_HasProto(getIOManagerEvent)          \
   SymI_HasProto(ioManagerFinished)          \
   SymI_HasProto(console_handler)
#endif

#define RTS_LIBFFI_SYMBOLS                                  \
     SymE_NeedsProto(ffi_prep_cif)                          \
     SymE_NeedsProto(ffi_call)                              \
     SymE_NeedsDataProto(ffi_type_void)                     \
     SymE_NeedsDataProto(ffi_type_float)                    \
     SymE_NeedsDataProto(ffi_type_double)                   \
     SymE_NeedsDataProto(ffi_type_sint64)                   \
     SymE_NeedsDataProto(ffi_type_uint64)                   \
     SymE_NeedsDataProto(ffi_type_sint32)                   \
     SymE_NeedsDataProto(ffi_type_uint32)                   \
     SymE_NeedsDataProto(ffi_type_sint16)                   \
     SymE_NeedsDataProto(ffi_type_uint16)                   \
     SymE_NeedsDataProto(ffi_type_sint8)                    \
     SymE_NeedsDataProto(ffi_type_uint8)                    \
     SymE_NeedsDataProto(ffi_type_pointer)

#if defined(TABLES_NEXT_TO_CODE)
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
/* This list should marry up with the one in $(TOP)/rts/include/stg/Ticky.h */
#define RTS_TICKY_SYMBOLS                               \
      SymI_NeedsDataProto(ticky_entry_ctrs)             \
      SymI_NeedsDataProto(top_ct)                       \
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
      SymI_HasProto(ENT_CONTINUATION_ctr)                  \
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
      SymI_HasProto(ALLOC_TSO_tot)                      \
      SymI_HasProto(ALLOC_STACK_ctr)                    \
      SymI_HasProto(ALLOC_STACK_tot)                    \
      SymI_HasProto(RET_NEW_ctr)                        \
      SymI_HasProto(RET_OLD_ctr)                        \
      SymI_HasProto(RET_UNBOXED_TUP_ctr)                \
      SymI_HasProto(RET_SEMI_loads_avoided)             \
                                                        \
      SymI_HasProto(TAG_UNTAGGED_pred)                  \
      SymI_HasProto(TAG_UNTAGGED_miss)                  \
      SymI_HasProto(TAG_TAGGED_pred)                    \
      SymI_HasProto(TAG_TAGGED_miss)                    \
                                                        \
      SymI_HasProto(RET_NEW_hst)                        \
      SymI_HasProto(RET_OLD_hst)                        \
      SymI_HasProto(RET_UNBOXED_TUP_hst)


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

#if defined(PROFILING)
#define RTS_PROF_SYMBOLS                        \
      SymI_HasProto(CCS_DONT_CARE)              \
      SymI_HasProto(CC_LIST)                    \
      SymI_HasProto(stg_restore_cccs_info)      \
      SymI_HasProto(enterFunCCS)                \
      SymI_HasProto(pushCostCentre)             \
      SymI_HasProto(mkCostCentre)               \
      SymI_HasProto(registerCcList)             \
      SymI_HasProto(registerCcsList)            \
      SymI_HasProto(era)                        \
      SymI_HasProto(user_era)
#else
#define RTS_PROF_SYMBOLS /* empty */
#endif

#define RTS_SYMBOLS                                                     \
      Maybe_Stable_Names                                                \
      RTS_TICKY_SYMBOLS                                                 \
      RTS_PROF_SYMBOLS                                                  \
      RTS_LIBDW_SYMBOLS                                                 \
      SymI_HasProto(StgReturn)                                          \
      SymI_HasDataProto(stg_gc_noregs)                                      \
      SymI_HasDataProto(stg_ret_v_info)                                     \
      SymI_HasDataProto(stg_ret_p_info)                                     \
      SymI_HasDataProto(stg_ret_n_info)                                     \
      SymI_HasDataProto(stg_ret_f_info)                                     \
      SymI_HasDataProto(stg_ret_d_info)                                     \
      SymI_HasDataProto(stg_ret_l_info)                                     \
      SymI_HasDataProto(stg_ret_t_info)                                     \
      SymI_HasDataProto(stg_ctoi_t)                                         \
      SymI_HasDataProto(stg_primcall_info)                                  \
      SymI_HasDataProto(stg_gc_prim_p)                                      \
      SymI_HasDataProto(stg_gc_prim_pp)                                     \
      SymI_HasDataProto(stg_gc_prim_n)                                      \
      SymI_HasDataProto(stg_enter_info)                                     \
      SymI_HasDataProto(__stg_gc_enter_1)                                   \
      SymI_HasDataProto(stg_gc_unpt_r1)                                     \
      SymI_HasDataProto(stg_gc_unbx_r1)                                     \
      SymI_HasDataProto(stg_gc_f1)                                          \
      SymI_HasDataProto(stg_gc_d1)                                          \
      SymI_HasDataProto(stg_gc_l1)                                          \
      SymI_HasDataProto(stg_gc_pp)                                          \
      SymI_HasDataProto(stg_gc_ppp)                                         \
      SymI_HasDataProto(stg_gc_pppp)                                        \
      SymI_HasDataProto(__stg_gc_fun)                                       \
      SymI_HasDataProto(stg_gc_fun_info)                                    \
      SymI_HasDataProto(stg_yield_noregs)                                   \
      SymI_HasDataProto(stg_yield_to_interpreter)                           \
      SymI_HasDataProto(stg_block_noregs)                                   \
      SymI_HasDataProto(stg_block_takemvar)                                 \
      SymI_HasDataProto(stg_block_readmvar)                                 \
      SymI_HasDataProto(stg_block_putmvar)                                  \
      MAIN_CAP_SYM                                                      \
      SymI_HasProto(addDLL)                                             \
      SymI_HasProto(addLibrarySearchPath)                               \
      SymI_HasProto(removeLibrarySearchPath)                            \
      SymI_HasProto(findSystemLibrary)                                  \
      SymI_HasProto(__int_encodeDouble)                                 \
      SymI_HasProto(__word_encodeDouble)                                \
      SymI_HasProto(__int_encodeFloat)                                  \
      SymI_HasProto(__word_encodeFloat)                                 \
      SymI_HasDataProto(stg_atomicallyzh)                                   \
      SymI_HasProto(barf)                                               \
      SymI_HasProto(flushEventLog)                                      \
      SymI_HasProto(deRefStablePtr)                                     \
      SymI_HasProto(debugBelch)                                         \
      SymI_HasProto(errorBelch)                                         \
      SymI_HasProto(sysErrorBelch)                                      \
      SymI_HasDataProto(stg_getMaskingStatezh)                              \
      SymI_HasDataProto(stg_maskAsyncExceptionszh)                          \
      SymI_HasDataProto(stg_maskUninterruptiblezh)                          \
      SymI_HasDataProto(stg_catchzh)                                        \
      SymI_HasDataProto(stg_catchRetryzh)                                   \
      SymI_HasDataProto(stg_catchSTMzh)                                     \
      SymI_HasDataProto(stg_clearCCSzh)                                     \
      SymI_HasDataProto(stg_compactAddWithSharingzh)                        \
      SymI_HasDataProto(stg_compactAddzh)                                   \
      SymI_HasDataProto(stg_compactNewzh)                                   \
      SymI_HasDataProto(stg_compactResizzezh)                               \
      SymI_HasDataProto(stg_compactContainszh)                              \
      SymI_HasDataProto(stg_compactContainsAnyzh)                           \
      SymI_HasDataProto(stg_compactGetFirstBlockzh)                         \
      SymI_HasDataProto(stg_compactGetNextBlockzh)                          \
      SymI_HasDataProto(stg_compactAllocateBlockzh)                         \
      SymI_HasDataProto(stg_compactFixupPointerszh)                         \
      SymI_HasDataProto(stg_compactSizzezh)                                 \
      SymI_HasProto(closure_flags)                                      \
      SymI_HasProto(eq_thread)                                          \
      SymI_HasProto(cmp_thread)                                         \
      SymI_HasProto(createAdjustor)                                     \
      SymI_HasDataProto(stg_decodeDoublezu2Intzh)                           \
      SymI_HasDataProto(stg_decodeDoublezuInt64zh)                          \
      SymI_HasDataProto(stg_decodeFloatzuIntzh)                             \
      SymI_HasDataProto(stg_delayzh)                                        \
      SymI_HasDataProto(stg_deRefWeakzh)                                    \
      SymI_HasDataProto(stg_deRefStablePtrzh)                               \
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
      SymI_HasProto(getRTSStats)                                        \
      SymI_HasProto(getRTSStatsEnabled)                                 \
      SymI_HasProto(getOrSetLibHSghcGlobalHasPprDebug)                  \
      SymI_HasProto(getOrSetLibHSghcGlobalHasNoDebugOutput)             \
      SymI_HasProto(getOrSetLibHSghcGlobalHasNoStateHack)               \
      SymI_HasProto(ghc_unique_counter64)                               \
      SymI_HasProto(ghc_unique_inc)                                     \
      SymI_HasProto(genericRaise)                                       \
      SymI_HasProto(getProgArgv)                                        \
      SymI_HasProto(getFullProgArgv)                                    \
      SymI_HasProto(setFullProgArgv)                                    \
      SymI_HasProto(freeFullProgArgv)                                   \
      SymI_HasProto(getProcessElapsedTime)                              \
      SymI_HasProto(getStablePtr)                                       \
      SymI_HasProto(registerForeignExports)                             \
      SymI_HasProto(hs_init)                                            \
      SymI_HasProto(hs_init_with_rtsopts)                               \
      SymI_HasProto(hs_init_ghc)                                        \
      SymI_HasProto(hs_exit)                                            \
      SymI_HasProto(hs_exit_nowait)                                     \
      SymI_HasProto(hs_set_argv)                                        \
      SymI_HasProto(hs_perform_gc)                                      \
      SymI_HasProto(hs_lock_stable_ptr_table)                           \
      SymI_HasProto(hs_unlock_stable_ptr_table)                         \
      SymI_HasProto(hs_lock_stable_tables)                              \
      SymI_HasProto(hs_unlock_stable_tables)                            \
      SymI_HasProto(hs_free_stable_ptr)                                 \
      SymI_HasProto(hs_free_stable_ptr_unsafe)                          \
      SymI_HasProto(hs_free_fun_ptr)                                    \
      SymI_HasProto(hs_hpc_rootModule)                                  \
      SymI_HasProto(hs_hpc_module)                                      \
      SymI_HasProto(hs_thread_done)                                     \
      SymI_HasProto(hs_try_putmvar)                                     \
      SymI_HasProto(defaultRtsConfig)                                   \
      SymI_HasProto(initLinker)                                         \
      SymI_HasProto(initLinker_)                                        \
      SymI_HasDataProto(stg_unpackClosurezh)                            \
      SymI_HasDataProto(stg_closureSizzezh)                                 \
      SymI_HasDataProto(stg_whereFromzh)                                 \
      SymI_HasDataProto(stg_getApStackValzh)                                \
      SymI_HasDataProto(stg_getSparkzh)                                     \
      SymI_HasDataProto(stg_numSparkszh)                                    \
      SymI_HasDataProto(stg_isCurrentThreadBoundzh)                         \
      SymI_HasDataProto(stg_isEmptyMVarzh)                                  \
      SymI_HasDataProto(stg_killThreadzh)                                   \
      SymI_HasDataProto(stg_listThreadszh)                                  \
      SymI_HasDataProto(stg_threadLabelzh)                                  \
      SymI_HasProto(loadArchive)                                        \
      SymI_HasProto(loadObj)                                            \
      SymI_HasProto(purgeObj)                                           \
      SymI_HasProto(insertSymbol)                                       \
      SymI_HasProto(lookupSymbol)                                       \
      SymI_HasDataProto(stg_makeStablePtrzh)                                \
      SymI_HasDataProto(stg_mkApUpd0zh)                                     \
      SymI_HasDataProto(stg_labelThreadzh)                                  \
      SymI_HasDataProto(stg_newArrayzh)                                     \
      SymI_HasDataProto(stg_copyArrayzh)                                    \
      SymI_HasDataProto(stg_copyMutableArrayzh)                             \
      SymI_HasDataProto(stg_cloneArrayzh)                                   \
      SymI_HasDataProto(stg_cloneMutableArrayzh)                            \
      SymI_HasDataProto(stg_freezzeArrayzh)                                 \
      SymI_HasDataProto(stg_thawArrayzh)                                    \
      SymI_HasDataProto(stg_casArrayzh)                                     \
      SymI_HasDataProto(stg_newSmallArrayzh)                                \
      SymI_HasDataProto(stg_unsafeThawSmallArrayzh)                         \
      SymI_HasDataProto(stg_cloneSmallArrayzh)                              \
      SymI_HasDataProto(stg_cloneSmallMutableArrayzh)                       \
      SymI_HasDataProto(stg_freezzeSmallArrayzh)                            \
      SymI_HasDataProto(stg_thawSmallArrayzh)                               \
      SymI_HasDataProto(stg_copySmallArrayzh)                               \
      SymI_HasDataProto(stg_copySmallMutableArrayzh)                        \
      SymI_HasDataProto(stg_casSmallArrayzh)                                \
      SymI_HasDataProto(stg_copyArray_barrier)                              \
      SymI_HasDataProto(stg_newBCOzh)                                       \
      SymI_HasDataProto(stg_newByteArrayzh)                                 \
      SymI_HasDataProto(stg_casIntArrayzh)                                  \
      SymI_HasDataProto(stg_casInt8Arrayzh)                                 \
      SymI_HasDataProto(stg_casInt16Arrayzh)                                \
      SymI_HasDataProto(stg_casInt32Arrayzh)                                \
      SymI_HasDataProto(stg_casInt64Arrayzh)                                \
      SymI_HasDataProto(stg_newMVarzh)                                      \
      SymI_HasDataProto(stg_newMutVarzh)                                    \
      SymI_HasDataProto(stg_newTVarzh)                                      \
      SymI_HasDataProto(stg_readIOPortzh)                                   \
      SymI_HasDataProto(stg_writeIOPortzh)                                  \
      SymI_HasDataProto(stg_newIOPortzh)                                    \
      SymI_HasDataProto(stg_noDuplicatezh)                                  \
      SymI_HasDataProto(stg_atomicModifyMutVar2zh)                          \
      SymI_HasDataProto(stg_atomicModifyMutVarzuzh)                         \
      SymI_HasDataProto(stg_casMutVarzh)                                    \
      SymI_HasDataProto(stg_newPinnedByteArrayzh)                           \
      SymI_HasDataProto(stg_newAlignedPinnedByteArrayzh)                    \
      SymI_HasDataProto(stg_isByteArrayPinnedzh)                            \
      SymI_HasDataProto(stg_isMutableByteArrayPinnedzh)                     \
      SymI_HasDataProto(stg_shrinkMutableByteArrayzh)                       \
      SymI_HasDataProto(stg_resizzeMutableByteArrayzh)                      \
      SymI_HasDataProto(stg_shrinkSmallMutableArrayzh)                       \
      SymI_HasProto(newSpark)                                           \
      SymI_HasProto(updateRemembSetPushThunk)                             \
      SymI_HasProto(updateRemembSetPushThunk_)                            \
      SymI_HasProto(updateRemembSetPushClosure_)                          \
      SymI_HasProto(performGC)                                          \
      SymI_HasProto(performMajorGC)                                     \
      SymI_HasProto(performBlockingMajorGC)                             \
      SymI_HasProto(prog_argc)                                          \
      SymI_HasProto(prog_argv)                                          \
      SymI_HasDataProto(stg_putMVarzh)                                      \
      SymI_HasDataProto(stg_raisezh)                                        \
      SymI_HasDataProto(stg_raiseDivZZerozh)                                \
      SymI_HasDataProto(stg_raiseUnderflowzh)                               \
      SymI_HasDataProto(stg_raiseOverflowzh)                                \
      SymI_HasDataProto(stg_raiseIOzh)                                      \
      SymI_HasDataProto(stg_keepAlivezh)                                    \
      SymI_HasDataProto(stg_paniczh)                                        \
      SymI_HasDataProto(stg_absentErrorzh)                                  \
      SymI_HasDataProto(stg_readTVarzh)                                     \
      SymI_HasDataProto(stg_readTVarIOzh)                                   \
      SymI_HasProto(resumeThread)                                       \
      SymI_HasProto(setNumCapabilities)                                 \
      SymI_HasProto(getNumberOfProcessors)                              \
      SymI_HasProto(resolveObjs)                                        \
      SymI_HasDataProto(stg_retryzh)                                        \
      SymI_HasProto(rts_apply)                                          \
      SymI_HasProto(rts_checkSchedStatus)                               \
      SymI_HasProto(rts_eval)                                           \
      SymI_HasProto(rts_evalIO)                                         \
      SymI_HasProto(rts_evalLazyIO)                                     \
      SymI_HasProto(rts_evalStableIOMain)                               \
      SymI_HasProto(rts_evalStableIO)                                   \
      SymI_HasProto(rts_eval_)                                          \
      SymI_HasProto(rts_inCall)                                         \
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
      SymI_HasProto(rts_isThreaded)                                     \
      SymI_HasProto(rts_isDebugged)                                     \
      SymI_HasProto(rts_isTracing)                                      \
      SymI_HasProto(rts_setInCallCapability)                            \
      SymI_HasProto(rts_enableThreadAllocationLimit)                    \
      SymI_HasProto(rts_disableThreadAllocationLimit)                   \
      SymI_HasProto(rts_setMainThread)                                  \
      SymI_HasProto(setProgArgv)                                        \
      SymI_HasProto(startupHaskell)                                     \
      SymI_HasProto(shutdownHaskell)                                    \
      SymI_HasProto(shutdownHaskellAndExit)                             \
      SymI_HasProto(stable_name_table)                                  \
      SymI_HasProto(stable_ptr_table)                                   \
      SymI_HasProto(reportStackOverflow)                                \
      SymI_HasProto(reportHeapOverflow)                                 \
      SymI_HasDataProto(stg_CAF_BLACKHOLE_info)                             \
      SymI_HasDataProto(stg_BLACKHOLE_info)                                 \
      SymI_HasDataProto(__stg_EAGER_BLACKHOLE_info)                         \
      SymI_HasDataProto(stg_BLOCKING_QUEUE_CLEAN_info)                      \
      SymI_HasDataProto(stg_BLOCKING_QUEUE_DIRTY_info)                      \
      SymI_HasProto(startTimer)                                         \
      SymI_HasDataProto(stg_MVAR_CLEAN_info)                                \
      SymI_HasDataProto(stg_MVAR_DIRTY_info)                                \
      SymI_HasDataProto(stg_TVAR_CLEAN_info)                                \
      SymI_HasDataProto(stg_TVAR_DIRTY_info)                                \
      SymI_HasDataProto(stg_IND_STATIC_info)                                \
      SymI_HasDataProto(stg_ARR_WORDS_info)                                 \
      SymI_HasDataProto(stg_MUT_ARR_PTRS_DIRTY_info)                        \
      SymI_HasDataProto(stg_MUT_ARR_PTRS_FROZEN_CLEAN_info)                 \
      SymI_HasDataProto(stg_MUT_ARR_PTRS_FROZEN_DIRTY_info)                 \
      SymI_HasDataProto(stg_SMALL_MUT_ARR_PTRS_DIRTY_info)                  \
      SymI_HasDataProto(stg_SMALL_MUT_ARR_PTRS_FROZEN_CLEAN_info)           \
      SymI_HasDataProto(stg_SMALL_MUT_ARR_PTRS_FROZEN_DIRTY_info)           \
      SymI_HasDataProto(stg_MUT_VAR_CLEAN_info)                             \
      SymI_HasDataProto(stg_MUT_VAR_DIRTY_info)                             \
      SymI_HasDataProto(stg_WEAK_info)                                      \
      SymI_HasDataProto(stg_SRT_1_info)                                     \
      SymI_HasDataProto(stg_SRT_2_info)                                     \
      SymI_HasDataProto(stg_SRT_3_info)                                     \
      SymI_HasDataProto(stg_SRT_4_info)                                     \
      SymI_HasDataProto(stg_SRT_5_info)                                     \
      SymI_HasDataProto(stg_SRT_6_info)                                     \
      SymI_HasDataProto(stg_SRT_7_info)                                     \
      SymI_HasDataProto(stg_SRT_8_info)                                     \
      SymI_HasDataProto(stg_SRT_9_info)                                     \
      SymI_HasDataProto(stg_SRT_10_info)                                    \
      SymI_HasDataProto(stg_SRT_11_info)                                    \
      SymI_HasDataProto(stg_SRT_12_info)                                    \
      SymI_HasDataProto(stg_SRT_13_info)                                    \
      SymI_HasDataProto(stg_SRT_14_info)                                    \
      SymI_HasDataProto(stg_SRT_15_info)                                    \
      SymI_HasDataProto(stg_SRT_16_info)                                    \
      SymI_HasDataProto(stg_ap_v_info)                                      \
      SymI_HasDataProto(stg_ap_f_info)                                      \
      SymI_HasDataProto(stg_ap_d_info)                                      \
      SymI_HasDataProto(stg_ap_l_info)                                      \
      SymI_HasDataProto(stg_ap_v16_info)                                    \
      SymI_HasDataProto(stg_ap_v32_info)                                    \
      SymI_HasDataProto(stg_ap_v64_info)                                    \
      SymI_HasDataProto(stg_ap_n_info)                                      \
      SymI_HasDataProto(stg_ap_p_info)                                      \
      SymI_HasDataProto(stg_ap_pv_info)                                     \
      SymI_HasDataProto(stg_ap_pp_info)                                     \
      SymI_HasDataProto(stg_ap_ppv_info)                                    \
      SymI_HasDataProto(stg_ap_ppp_info)                                    \
      SymI_HasDataProto(stg_ap_pppv_info)                                   \
      SymI_HasDataProto(stg_ap_pppp_info)                                   \
      SymI_HasDataProto(stg_ap_ppppp_info)                                  \
      SymI_HasDataProto(stg_ap_pppppp_info)                                 \
      SymI_HasDataProto(stg_ap_0_fast)                                      \
      SymI_HasDataProto(stg_ap_v_fast)                                      \
      SymI_HasDataProto(stg_ap_f_fast)                                      \
      SymI_HasDataProto(stg_ap_d_fast)                                      \
      SymI_HasDataProto(stg_ap_l_fast)                                      \
      SymI_HasDataProto(stg_ap_v16_fast)                                    \
      SymI_HasDataProto(stg_ap_v32_fast)                                    \
      SymI_HasDataProto(stg_ap_v64_fast)                                    \
      SymI_HasDataProto(stg_ap_n_fast)                                      \
      SymI_HasDataProto(stg_ap_p_fast)                                      \
      SymI_HasDataProto(stg_ap_pv_fast)                                     \
      SymI_HasDataProto(stg_ap_pp_fast)                                     \
      SymI_HasDataProto(stg_ap_ppv_fast)                                    \
      SymI_HasDataProto(stg_ap_ppp_fast)                                    \
      SymI_HasDataProto(stg_ap_pppv_fast)                                   \
      SymI_HasDataProto(stg_ap_pppp_fast)                                   \
      SymI_HasDataProto(stg_ap_ppppp_fast)                                  \
      SymI_HasDataProto(stg_ap_pppppp_fast)                                 \
      SymI_HasDataProto(stg_ap_1_upd_info)                                  \
      SymI_HasDataProto(stg_ap_2_upd_info)                                  \
      SymI_HasDataProto(stg_ap_3_upd_info)                                  \
      SymI_HasDataProto(stg_ap_4_upd_info)                                  \
      SymI_HasDataProto(stg_ap_5_upd_info)                                  \
      SymI_HasDataProto(stg_ap_6_upd_info)                                  \
      SymI_HasDataProto(stg_ap_7_upd_info)                                  \
      SymI_HasDataProto(stg_exit)                                           \
      SymI_HasDataProto(stg_sel_0_upd_info)                                 \
      SymI_HasDataProto(stg_sel_1_upd_info)                                 \
      SymI_HasDataProto(stg_sel_2_upd_info)                                 \
      SymI_HasDataProto(stg_sel_3_upd_info)                                 \
      SymI_HasDataProto(stg_sel_4_upd_info)                                 \
      SymI_HasDataProto(stg_sel_5_upd_info)                                 \
      SymI_HasDataProto(stg_sel_6_upd_info)                                 \
      SymI_HasDataProto(stg_sel_7_upd_info)                                 \
      SymI_HasDataProto(stg_sel_8_upd_info)                                 \
      SymI_HasDataProto(stg_sel_9_upd_info)                                 \
      SymI_HasDataProto(stg_sel_10_upd_info)                                \
      SymI_HasDataProto(stg_sel_11_upd_info)                                \
      SymI_HasDataProto(stg_sel_12_upd_info)                                \
      SymI_HasDataProto(stg_sel_13_upd_info)                                \
      SymI_HasDataProto(stg_sel_14_upd_info)                                \
      SymI_HasDataProto(stg_sel_15_upd_info)                                \
      SymI_HasDataProto(stg_sel_0_noupd_info)                               \
      SymI_HasDataProto(stg_sel_1_noupd_info)                               \
      SymI_HasDataProto(stg_sel_2_noupd_info)                               \
      SymI_HasDataProto(stg_sel_3_noupd_info)                               \
      SymI_HasDataProto(stg_sel_4_noupd_info)                               \
      SymI_HasDataProto(stg_sel_5_noupd_info)                               \
      SymI_HasDataProto(stg_sel_6_noupd_info)                               \
      SymI_HasDataProto(stg_sel_7_noupd_info)                               \
      SymI_HasDataProto(stg_sel_8_noupd_info)                               \
      SymI_HasDataProto(stg_sel_9_noupd_info)                               \
      SymI_HasDataProto(stg_sel_10_noupd_info)                              \
      SymI_HasDataProto(stg_sel_11_noupd_info)                              \
      SymI_HasDataProto(stg_sel_12_noupd_info)                              \
      SymI_HasDataProto(stg_sel_13_noupd_info)                              \
      SymI_HasDataProto(stg_sel_14_noupd_info)                              \
      SymI_HasDataProto(stg_sel_15_noupd_info)                              \
      SymI_HasDataProto(stg_unpack_cstring_info)                            \
      SymI_HasDataProto(stg_unpack_cstring_utf8_info)                       \
      SymI_HasDataProto(stg_upd_frame_info)                                 \
      SymI_HasDataProto(stg_bh_upd_frame_info)                              \
      SymI_HasDataProto(stg_orig_thunk_info_frame_info)                     \
      SymI_HasProto(suspendThread)                                          \
      SymI_HasDataProto(stg_takeMVarzh)                                     \
      SymI_HasDataProto(stg_readMVarzh)                                     \
      SymI_HasDataProto(stg_threadStatuszh)                                 \
      SymI_HasDataProto(stg_tryPutMVarzh)                                   \
      SymI_HasDataProto(stg_tryTakeMVarzh)                                  \
      SymI_HasDataProto(stg_tryReadMVarzh)                                  \
      SymI_HasDataProto(stg_unmaskAsyncExceptionszh)                        \
      SymI_HasProto(unloadObj)                                              \
      SymI_HasDataProto(stg_unsafeThawArrayzh)                              \
      SymI_HasDataProto(stg_waitReadzh)                                     \
      SymI_HasDataProto(stg_waitWritezh)                                    \
      SymI_HasDataProto(stg_writeTVarzh)                                    \
      SymI_HasDataProto(stg_yieldzh)                                        \
      SymI_NeedsProto(stg_badAlignment_entry)                           \
      SymI_NeedsProto(stg_interp_constr1_entry)                         \
      SymI_NeedsProto(stg_interp_constr2_entry)                         \
      SymI_NeedsProto(stg_interp_constr3_entry)                         \
      SymI_NeedsProto(stg_interp_constr4_entry)                         \
      SymI_NeedsProto(stg_interp_constr5_entry)                         \
      SymI_NeedsProto(stg_interp_constr6_entry)                         \
      SymI_NeedsProto(stg_interp_constr7_entry)                         \
      SymI_HasDataProto(stg_arg_bitmaps)                                \
      SymI_HasProto(large_alloc_lim)                                    \
      SymI_HasProto(g0)                                                 \
      SymI_HasProto(allocate)                                           \
      SymI_HasProto(allocateExecPage)                                   \
      SymI_HasProto(freezeExecPage)                                     \
      SymI_HasProto(freeExecPage)                                       \
      SymI_HasProto(getAllocations)                                     \
      SymI_HasProto(revertCAFs)                                         \
      SymI_HasProto(RtsFlags)                                           \
      SymI_NeedsDataProto(rts_breakpoint_io_action)                     \
      SymI_NeedsDataProto(rts_stop_next_breakpoint)                     \
      SymI_NeedsDataProto(rts_stop_on_exception)                        \
      SymI_HasProto(stopTimer)                                          \
      SymI_HasProto(n_capabilities)                                     \
      SymI_HasProto(enabled_capabilities)                               \
      SymI_HasDataProto(stg_traceEventzh)                                   \
      SymI_HasDataProto(stg_traceMarkerzh)                                  \
      SymI_HasDataProto(stg_traceBinaryEventzh)                             \
      SymI_HasDataProto(stg_getThreadAllocationCounterzh)                   \
      SymI_HasDataProto(stg_setThreadAllocationCounterzh)                   \
      SymI_HasProto(getMonotonicNSec)                                   \
      SymI_HasProto(lockFile)                                           \
      SymI_HasProto(unlockFile)                                         \
      SymI_HasProto(startProfTimer)                                     \
      SymI_HasProto(stopProfTimer)                                      \
      SymI_HasProto(startHeapProfTimer)                                 \
      SymI_HasProto(stopHeapProfTimer)                                  \
      SymI_HasProto(setUserEra)                                         \
      SymI_HasProto(incrementUserEra)                                   \
      SymI_HasProto(getUserEra)                                         \
      SymI_HasProto(requestHeapCensus)                                  \
      SymI_HasProto(atomic_inc)                                         \
      SymI_HasProto(atomic_dec)                                         \
      SymI_HasProto(hs_spt_lookup)                                      \
      SymI_HasProto(hs_spt_insert)                                      \
      SymI_HasProto(hs_spt_insert_stableptr)                            \
      SymI_HasProto(hs_spt_remove)                                      \
      SymI_HasProto(hs_spt_keys)                                        \
      SymI_HasProto(hs_spt_key_count)                                   \
      SymI_HasProto(cas)                                                \
      SymI_HasProto(_assertFail)                                        \
      SymI_HasProto(keepCAFs)                                           \
      SymI_HasProto(registerInfoProvList)                               \
      SymI_HasProto(lookupIPE)                                          \
      SymI_HasProto(sendCloneStackMessage)                              \
      SymI_HasProto(cloneStack)                                         \
      SymI_HasProto(decodeClonedStack)                                  \
      SymI_HasProto(stg_newPromptTagzh)                                 \
      SymI_HasProto(stg_promptzh)                                       \
      SymI_HasProto(stg_control0zh)                                     \
      SymI_HasProto(newArena)                                           \
      SymI_HasProto(arenaAlloc)                                         \
      SymI_HasProto(arenaFree)                                          \
      SymI_HasProto(rts_clearMemory)                                    \
      SymI_HasProto(setKeepCAFs)                                        \
      SymI_HasProto(rtsBadAlignmentBarf)                                \
      SymI_HasProto(rtsOutOfBoundsAccess)                               \
      SymI_HasProto(rtsMemcpyRangeOverlap)                              \
      SymI_HasDataProto(stg_castWord64ToDoublezh)                       \
      SymI_HasDataProto(stg_castDoubleToWord64zh)                       \
      SymI_HasDataProto(stg_castWord32ToFloatzh)                        \
      SymI_HasDataProto(stg_castFloatToWord32zh)                        \
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
#elif defined(__GNUC__) && SIZEOF_VOID_P == 8
#define RTS_LIBGCC_SYMBOLS                             \
      SymI_NeedsProto(__udivti3)                       \
      SymI_NeedsProto(__umodti3)
#else
#define RTS_LIBGCC_SYMBOLS
#endif

#if defined(riscv64_HOST_ARCH)
#define RTS_ARCH_LIBGCC_SYMBOLS \
  SymI_NeedsProto(__clzdi2)
#else
#define RTS_ARCH_LIBGCC_SYMBOLS
#endif

// Symbols defined by libgcc/compiler-rt for AArch64's outline atomics.
#if defined(HAVE_ARM_OUTLINE_ATOMICS)
#include "ARMOutlineAtomicsSymbols.h"
#else
#define RTS_ARM_OUTLINE_ATOMIC_SYMBOLS
#endif

// Symbols defined by libc
#define RTS_LIBC_SYMBOLS                               \
      SymI_HasProto_redirect(atexit, atexit, STRENGTH_STRONG, SYM_TYPE_CODE) /* See Note [Strong symbols] */ \
      SymI_HasProto(environ)

#if !defined(DYNAMIC) && defined(linux_HOST_OS)
// we need these for static musl builds. However when
// linking shared objects (DLLs) this will fail, hence
// we do not include them when building with -DDYNAMIC
#define RTS_FINI_ARRAY_SYMBOLS                         \
      SymI_NeedsProto(__fini_array_start)              \
      SymI_NeedsProto(__fini_array_end)
#else
#define RTS_FINI_ARRAY_SYMBOLS
#endif

/* entirely bogus claims about types of these symbols */
#define SymI_NeedsProto(vvv)  extern void vvv(void);
#define SymI_NeedsDataProto(vvv)  extern StgWord vvv[];
#if defined(COMPILING_WINDOWS_DLL)
#define SymE_HasProto(vvv)    SymE_HasProto(vvv);
#  if defined(x86_64_HOST_ARCH)
#    define SymE_NeedsProto(vvv)    extern void __imp_ ## vvv (void);
#    define SymE_NeedsDataProto(vvv) SymE_NeedsProto(vvv)
#  else
#    define SymE_NeedsProto(vvv)    extern void _imp__ ## vvv (void);
#    define SymE_NeedsDataProto(vvv) SymE_NeedsProto(vvv)
#  endif
#else
#define SymE_NeedsProto(vvv)  SymI_NeedsProto(vvv);
#define SymE_NeedsDataProto(vvv)  SymI_NeedsDataProto(vvv);
#define SymE_HasProto(vvv)    SymI_HasProto(vvv);
#endif
#define SymI_HasProto(vvv) /**/
#define SymI_HasDataProto(vvv) /**/
#define SymI_HasProto_redirect(vvv,xxx,strength,ty) /**/

RTS_SYMBOLS
RTS_RET_SYMBOLS
RTS_POSIX_ONLY_SYMBOLS
RTS_MINGW_ONLY_SYMBOLS
RTS_DARWIN_ONLY_SYMBOLS
RTS_OPENBSD_ONLY_SYMBOLS
RTS_LIBC_SYMBOLS
RTS_LIBGCC_SYMBOLS
RTS_ARCH_LIBGCC_SYMBOLS
RTS_FINI_ARRAY_SYMBOLS
RTS_LIBFFI_SYMBOLS
RTS_ARM_OUTLINE_ATOMIC_SYMBOLS

#undef SymI_NeedsProto
#undef SymI_NeedsDataProto
#undef SymI_HasProto
#undef SymI_HasDataProto
#undef SymI_HasProto_redirect
#undef SymE_HasProto
#undef SymE_HasDataProto
#undef SymE_NeedsProto
#undef SymE_NeedsDataProto

#define SymI_HasProto(vvv) { MAYBE_LEADING_UNDERSCORE_STR(#vvv), \
                    (void*)(&(vvv)), STRENGTH_NORMAL, SYM_TYPE_CODE },
#define SymI_HasDataProto(vvv) { MAYBE_LEADING_UNDERSCORE_STR(#vvv), \
                    (void*)(&(vvv)), STRENGTH_NORMAL, SYM_TYPE_DATA },
#define SymE_HasProto(vvv) { MAYBE_LEADING_UNDERSCORE_STR(#vvv), \
            (void*)DLL_IMPORT_DATA_REF(vvv), STRENGTH_NORMAL, SYM_TYPE_CODE },
#define SymE_HasDataProto(vvv) { MAYBE_LEADING_UNDERSCORE_STR(#vvv), \
            (void*)DLL_IMPORT_DATA_REF(vvv), STRENGTH_NORMAL, SYM_TYPE_DATA },

#define SymI_NeedsProto(vvv) SymI_HasProto(vvv)
#define SymI_NeedsDataProto(vvv) SymI_HasDataProto(vvv)
#define SymE_NeedsProto(vvv) SymE_HasProto(vvv)
#define SymE_NeedsDataProto(vvv) SymE_HasDataProto(vvv)

// SymI_HasProto_redirect allows us to redirect references to one symbol to
// another symbol.  See newCAF/newRetainedCAF/newGCdCAF for an example.
#define SymI_HasProto_redirect(vvv,xxx,strength,ty) \
    { MAYBE_LEADING_UNDERSCORE_STR(#vvv),    \
      (void*)(&(xxx)), strength, ty },

RtsSymbolVal rtsSyms[] = {
      RTS_SYMBOLS
      RTS_RET_SYMBOLS
      RTS_POSIX_ONLY_SYMBOLS
      RTS_MINGW_ONLY_SYMBOLS
      RTS_DARWIN_ONLY_SYMBOLS
      RTS_OPENBSD_ONLY_SYMBOLS
      RTS_LIBGCC_SYMBOLS
      RTS_ARCH_LIBGCC_SYMBOLS
      RTS_FINI_ARRAY_SYMBOLS
      RTS_LIBFFI_SYMBOLS
      RTS_ARM_OUTLINE_ATOMIC_SYMBOLS
      SymI_HasDataProto(nonmoving_write_barrier_enabled)
#if defined(darwin_HOST_OS) && defined(i386_HOST_ARCH)
      // dyld stub code contains references to this,
      // but it should never be called because we treat
      // lazy pointers as nonlazy.
      { "dyld_stub_binding_helper", (void*)0xDEADBEEF, STRENGTH_NORMAL },
#endif
      { 0, 0, STRENGTH_NORMAL, SYM_TYPE_CODE } /* sentinel */
};
