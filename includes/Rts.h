/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * RTS external APIs.  This file declares everything that the GHC RTS
 * exposes externally.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(__cplusplus)
extern "C" {
#endif

/* We include windows.h very early, as on Win64 the CONTEXT type has
   fields "R8", "R9" and "R10", which goes bad if we've already
   #define'd those names for our own purposes (in stg/Regs.h) */
#if defined(HAVE_WINDOWS_H)
#include <windows.h>
#endif

#if !defined(IN_STG_CODE)
#define IN_STG_CODE 0
#endif
#include "Stg.h"

#include "HsFFI.h"
#include "RtsAPI.h"

// Turn off inlining when debugging - it obfuscates things
#if defined(DEBUG)
# undef  STATIC_INLINE
# define STATIC_INLINE static
#endif

#include "rts/Types.h"
#include "rts/Time.h"

#if __GNUC__ >= 3
#define ATTRIBUTE_ALIGNED(n) __attribute__((aligned(n)))
#else
#define ATTRIBUTE_ALIGNED(n) /*nothing*/
#endif

// Symbols that are extern, but private to the RTS, are declared
// with visibility "hidden" to hide them outside the RTS shared
// library.
#if defined(HAS_VISIBILITY_HIDDEN)
#define RTS_PRIVATE  GNUC3_ATTRIBUTE(visibility("hidden"))
#else
#define RTS_PRIVATE  /* disabled: RTS_PRIVATE */
#endif

#if __GNUC__ >= 4
#define RTS_UNLIKELY(p) __builtin_expect((p),0)
#else
#define RTS_UNLIKELY(p) p
#endif

/* __builtin_unreachable is supported since GNU C 4.5 */
#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 5)
#define RTS_UNREACHABLE __builtin_unreachable()
#else
#define RTS_UNREACHABLE abort()
#endif

/* Fix for mingw stat problem (done here so it's early enough) */
#if defined(mingw32_HOST_OS)
#define __MSVCRT__ 1
#endif

/* Needed to get the macro version of errno on some OSs, and also to
   get prototypes for the _r versions of C library functions. */
#if !defined(_REENTRANT)
#define _REENTRANT 1
#endif

/*
 * We often want to know the size of something in units of an
 * StgWord... (rounded up, of course!)
 */
#define ROUNDUP_BYTES_TO_WDS(n) (((n) + sizeof(W_) - 1) / sizeof(W_))

#define sizeofW(t) ROUNDUP_BYTES_TO_WDS(sizeof(t))

/* -----------------------------------------------------------------------------
   Assertions and Debuggery

   CHECK(p)   evaluates p and terminates with an error if p is false
   ASSERT(p)  like CHECK(p) if DEBUG is on, otherwise a no-op
   -------------------------------------------------------------------------- */

void _assertFail(const char *filename, unsigned int linenum)
   GNUC3_ATTRIBUTE(__noreturn__);

#define CHECK(predicate)                        \
        if (predicate)                          \
            /*null*/;                           \
        else                                    \
            _assertFail(__FILE__, __LINE__)

#define CHECKM(predicate, msg, ...)             \
        if (predicate)                          \
            /*null*/;                           \
        else                                    \
            barf(msg, ##__VA_ARGS__)

#if !defined(DEBUG)
#define ASSERT(predicate) /* nothing */
#define ASSERTM(predicate,msg,...) /* nothing */
#else
#define ASSERT(predicate) CHECK(predicate)
#define ASSERTM(predicate,msg,...) CHECKM(predicate,msg,##__VA_ARGS__)
#endif /* DEBUG */

/*
 * Use this on the RHS of macros which expand to nothing
 * to make sure that the macro can be used in a context which
 * demands a non-empty statement.
 */

#define doNothing() do { } while (0)

#if defined(DEBUG)
#define USED_IF_DEBUG
#define USED_IF_NOT_DEBUG STG_UNUSED
#else
#define USED_IF_DEBUG STG_UNUSED
#define USED_IF_NOT_DEBUG
#endif

#if defined(THREADED_RTS)
#define USED_IF_THREADS
#define USED_IF_NOT_THREADS STG_UNUSED
#else
#define USED_IF_THREADS STG_UNUSED
#define USED_IF_NOT_THREADS
#endif

#if defined(PROFILING)
#define USED_IF_PROFILING
#define USED_IF_NOT_PROFILING STG_UNUSED
#else
#define USED_IF_PROFILING STG_UNUSED
#define USED_IF_NOT_PROFILING
#endif

#define FMT_SizeT    "zu"
#define FMT_HexSizeT "zx"

/* -----------------------------------------------------------------------------
   Include everything STG-ish
   -------------------------------------------------------------------------- */

/* System headers: stdlib.h is needed so that we can use NULL.  It must
 * come after MachRegs.h, because stdlib.h might define some inline
 * functions which may only be defined after register variables have
 * been declared.
 */
#include <stdlib.h>

#include "rts/Config.h"

/* Global constraints */
#include "rts/Constants.h"

/* Profiling information */
#include "rts/prof/CCS.h"
#include "rts/prof/LDV.h"

/* Parallel information */
#include "rts/OSThreads.h"
#include "rts/SpinLock.h"

#include "rts/Messages.h"
#include "rts/Threads.h"

/* Storage format definitions */
#include "rts/storage/FunTypes.h"
#include "rts/storage/InfoTables.h"
#include "rts/storage/Closures.h"
#include "rts/storage/Heap.h"
#include "rts/storage/ClosureTypes.h"
#include "rts/storage/TSO.h"
#include "stg/MiscClosures.h" /* InfoTables, closures etc. defined in the RTS */
#include "rts/storage/Block.h"
#include "rts/storage/ClosureMacros.h"
#include "rts/storage/MBlock.h"
#include "rts/storage/GC.h"

/* Other RTS external APIs */
#include "rts/Parallel.h"
#include "rts/Signals.h"
#include "rts/BlockSignals.h"
#include "rts/Hpc.h"
#include "rts/Flags.h"
#include "rts/Adjustor.h"
#include "rts/FileLock.h"
#include "rts/GetTime.h"
#include "rts/Globals.h"
#include "rts/IOManager.h"
#include "rts/Linker.h"
#include "rts/Ticky.h"
#include "rts/Timer.h"
#include "rts/StablePtr.h"
#include "rts/StableName.h"
#include "rts/TTY.h"
#include "rts/Utils.h"
#include "rts/PrimFloat.h"
#include "rts/Main.h"
#include "rts/Profiling.h"
#include "rts/StaticPtrTable.h"
#include "rts/Libdw.h"
#include "rts/LibdwPool.h"

/* Misc stuff without a home */
DLL_IMPORT_RTS extern char **prog_argv; /* so we can get at these from Haskell */
DLL_IMPORT_RTS extern int    prog_argc;
DLL_IMPORT_RTS extern char  *prog_name;

void reportStackOverflow(StgTSO* tso);
void reportHeapOverflow(void);

void stg_exit(int n) GNU_ATTRIBUTE(__noreturn__);

#if !defined(mingw32_HOST_OS)
int stg_sig_install (int, int, void *);
#endif

/* -----------------------------------------------------------------------------
   Ways
   -------------------------------------------------------------------------- */

// Returns non-zero if the RTS is a profiling version
int rts_isProfiled(void);

// Returns non-zero if the RTS is a dynamically-linked version
int rts_isDynamic(void);

/* -----------------------------------------------------------------------------
   RTS Exit codes
   -------------------------------------------------------------------------- */

/* 255 is allegedly used by dynamic linkers to report linking failure */
#define EXIT_INTERNAL_ERROR 254
#define EXIT_DEADLOCK       253
#define EXIT_INTERRUPTED    252
#define EXIT_HEAPOVERFLOW   251
#define EXIT_KILLED         250

/* -----------------------------------------------------------------------------
   Miscellaneous garbage
   -------------------------------------------------------------------------- */

#if defined(DEBUG)
#define TICK_VAR(arity) \
  extern StgInt SLOW_CALLS_##arity; \
  extern StgInt RIGHT_ARITY_##arity; \
  extern StgInt TAGGED_PTR_##arity;

extern StgInt TOTAL_CALLS;

TICK_VAR(1)
TICK_VAR(2)
#endif

/* -----------------------------------------------------------------------------
   Assertions and Debuggery
   -------------------------------------------------------------------------- */

#define IF_RTSFLAGS(c,s)  if (RtsFlags.c) { s; } doNothing()

#if defined(DEBUG)
#if IN_STG_CODE
#define IF_DEBUG(c,s)  if (RtsFlags[0].DebugFlags.c) { s; } doNothing()
#else
#define IF_DEBUG(c,s)  if (RtsFlags.DebugFlags.c) { s; } doNothing()
#endif
#else
#define IF_DEBUG(c,s)  doNothing()
#endif

#if defined(DEBUG)
#define DEBUG_ONLY(s) s
#else
#define DEBUG_ONLY(s) doNothing()
#endif

#if defined(DEBUG)
#define DEBUG_IS_ON   1
#else
#define DEBUG_IS_ON   0
#endif

/* -----------------------------------------------------------------------------
   Useful macros and inline functions
   -------------------------------------------------------------------------- */

#if defined(__GNUC__)
#define SUPPORTS_TYPEOF
#endif

#if defined(SUPPORTS_TYPEOF)
#define stg_min(a,b) ({typeof(a) _a = (a), _b = (b); _a <= _b ? _a : _b; })
#define stg_max(a,b) ({typeof(a) _a = (a), _b = (b); _a <= _b ? _b : _a; })
#else
#define stg_min(a,b) ((a) <= (b) ? (a) : (b))
#define stg_max(a,b) ((a) <= (b) ? (b) : (a))
#endif

/* -------------------------------------------------------------------------- */

#if defined(__cplusplus)
}
#endif
