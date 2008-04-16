/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Top-level include file for the RTS itself
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_H
#define RTS_H

#ifdef __cplusplus
extern "C" {
#endif

#ifndef IN_STG_CODE
#define IN_STG_CODE 0
#endif
#include "Stg.h"

// ToDo: move RtsExternal stuff elsewhere
#include "RtsExternal.h"

// Turn off inlining when debugging - it obfuscates things
#ifdef DEBUG
# undef  STATIC_INLINE
# define STATIC_INLINE static
#endif

#include "RtsTypes.h"

#if __GNUC__ >= 3
/* Assume that a flexible array member at the end of a struct
 * can be defined thus: T arr[]; */
#define FLEXIBLE_ARRAY
#else
/* Assume that it must be defined thus: T arr[0]; */
#define FLEXIBLE_ARRAY 0
#endif

#if __GNUC__ >= 3
#define ATTRIBUTE_ALIGNED(n) __attribute__((aligned(n)))
#else
#define ATTRIBUTE_ALIGNED(n) /*nothing*/
#endif

/* Fix for mingw stat problem (done here so it's early enough) */
#ifdef mingw32_HOST_OS
#define __MSVCRT__ 1
#endif

/* Needed to get the macro version of errno on some OSs, and also to
   get prototypes for the _r versions of C library functions. */
#define _REENTRANT 1

/*
 * We often want to know the size of something in units of an
 * StgWord... (rounded up, of course!)
 */
#define ROUNDUP_BYTES_TO_WDS(n) (((n) + sizeof(W_) - 1) / sizeof(W_))

#define sizeofW(t) ROUNDUP_BYTES_TO_WDS(sizeof(t))

/* 
 * It's nice to be able to grep for casts
 */
#define stgCast(ty,e) ((ty)(e))

/* -----------------------------------------------------------------------------
   Assertions and Debuggery
   -------------------------------------------------------------------------- */

#ifndef DEBUG
#define ASSERT(predicate) /* nothing */
#else

extern void _assertFail (const char *, unsigned int);

#define ASSERT(predicate)			\
	if (predicate)				\
	    /*null*/;				\
	else					\
	    _assertFail(__FILE__, __LINE__)
#endif /* DEBUG */

/* 
 * Use this on the RHS of macros which expand to nothing
 * to make sure that the macro can be used in a context which
 * demands a non-empty statement.
 */

#define doNothing() do { } while (0)

#ifdef DEBUG
#define USED_IF_DEBUG
#define USED_IF_NOT_DEBUG STG_UNUSED
#else
#define USED_IF_DEBUG STG_UNUSED
#define USED_IF_NOT_DEBUG
#endif

#ifdef THREADED_RTS
#define USED_IF_THREADS
#define USED_IF_NOT_THREADS STG_UNUSED
#else
#define USED_IF_THREADS STG_UNUSED
#define USED_IF_NOT_THREADS
#endif

/*
 * Getting printf formats right for platform-dependent typedefs
 */
#if SIZEOF_LONG == 8
#define FMT_Word64 "lu"
#define FMT_Int64  "ld"
#else
#define FMT_Word64 "llu"
#define FMT_Int64  "lld"
#endif

/*
 * Macros for untagging and retagging closure pointers
 * For more information look at the comments in Cmm.h
 */

static inline StgWord
GET_CLOSURE_TAG(StgClosure * p)
{
    return (StgWord)p & TAG_MASK;
}

static inline StgClosure *
UNTAG_CLOSURE(StgClosure * p)
{
    return (StgClosure*)((StgWord)p & ~TAG_MASK);
}

static inline StgClosure *
TAG_CLOSURE(StgWord tag,StgClosure * p)
{
    return (StgClosure*)((StgWord)p | tag);
}

/* -----------------------------------------------------------------------------
   Include everything STG-ish
   -------------------------------------------------------------------------- */

/* System headers: stdlib.h is eeded so that we can use NULL.  It must
 * come after MachRegs.h, because stdlib.h might define some inline
 * functions which may only be defined after register variables have
 * been declared.
 */
#include <stdlib.h>

/* Global constaints */
#include "Constants.h"

/* Profiling information */
#include "StgProf.h"
#include "StgLdvProf.h"

/* Storage format definitions */
#include "StgFun.h"
#include "Closures.h"
#include "Liveness.h"
#include "ClosureTypes.h"
#include "InfoTables.h"
#include "TSO.h"

/* Info tables, closures & code fragments defined in the RTS */
#include "StgMiscClosures.h"

/* Simulated-parallel information */
#include "GranSim.h"

/* Parallel information */
#include "Parallel.h"
#include "OSThreads.h"
#include "SMPClosureOps.h"
#include "SpinLock.h"

/* GNU mp library */
#if defined(HAVE_FRAMEWORK_GMP)
#include <GMP/gmp.h>
#else
#include "gmp.h"
#endif

/* Macros for STG/C code */
#include "Block.h"
#include "ClosureMacros.h"

  /* Ticky-ticky counters */
#include "TickyCounters.h"

/* Runtime-system hooks */
#include "Hooks.h"
#include "RtsMessages.h"

/* for StablePtr/getStablePtr/deRefStablePtr */
#include "Storage.h"
#include "Stable.h"

#include "ieee-flpt.h"

#include "Signals.h"

/* Misc stuff without a home */
DLL_IMPORT_RTS extern char **prog_argv;	/* so we can get at these from Haskell */
DLL_IMPORT_RTS extern int    prog_argc;
DLL_IMPORT_RTS extern char  *prog_name;

extern void stackOverflow(void);

extern void      __decodeDouble (MP_INT *man, I_ *_exp, StgDouble dbl);
extern void      __decodeFloat  (MP_INT *man, I_ *_exp, StgFloat flt);
extern void      __decodeDouble_2Int (I_ *man_sign, W_ *man_high, W_ *man_low, I_ *exp, StgDouble dbl);
extern void      __decodeFloat_Int (I_ *man, I_ *exp, StgFloat flt);

#if defined(WANT_DOTNET_SUPPORT)
#include "DNInvoke.h"
#endif

/* Initialising the whole adjustor thunk machinery. */
extern void initAdjustor(void);

extern void stg_exit(int n) GNU_ATTRIBUTE(__noreturn__);

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

/* declarations for runtime flags/values */
#define MAX_RTS_ARGS 32

#ifdef DEBUG
#define TICK_VAR(arity) \
  extern StgInt SLOW_CALLS_##arity; \
  extern StgInt RIGHT_ARITY_##arity; \
  extern StgInt TAGGED_PTR_##arity;

#define TICK_VAR_INI(arity) \
  StgInt SLOW_CALLS_##arity = 1; \
  StgInt RIGHT_ARITY_##arity = 1; \
  StgInt TAGGED_PTR_##arity = 0;

extern StgInt TOTAL_CALLS;

TICK_VAR(1)
TICK_VAR(2)
#endif

/* -----------------------------------------------------------------------------
   Assertions and Debuggery
   -------------------------------------------------------------------------- */

#define IF_RTSFLAGS(c,s)  if (RtsFlags.c) { s; }

/* -----------------------------------------------------------------------------
   Assertions and Debuggery
   -------------------------------------------------------------------------- */

#ifdef DEBUG
#if IN_STG_CODE
#define IF_DEBUG(c,s)  if (RtsFlags[0].DebugFlags.c) { s; }
#else
#define IF_DEBUG(c,s)  if (RtsFlags.DebugFlags.c) { s; }
#endif
#else
#define IF_DEBUG(c,s)  doNothing()
#endif

#ifdef DEBUG
#define DEBUG_ONLY(s) s
#else
#define DEBUG_ONLY(s) doNothing()
#endif

#if defined(GRAN) && defined(DEBUG)
#define IF_GRAN_DEBUG(c,s)  if (RtsFlags.GranFlags.Debug.c) { s; }
#else
#define IF_GRAN_DEBUG(c,s)  doNothing()
#endif

#if defined(PAR) && defined(DEBUG)
#define IF_PAR_DEBUG(c,s)  if (RtsFlags.ParFlags.Debug.c) { s; }
#else
#define IF_PAR_DEBUG(c,s)  doNothing()
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

#ifdef __cplusplus
}
#endif


/* krc: I put this here because I don't think
   it needs to be visible externally.
   It used to be in StgTicky.h, but I got rid
   of that. */

/* -----------------------------------------------------------------------------
   The StgEntCounter type - needed regardless of TICKY_TICKY
   -------------------------------------------------------------------------- */

typedef struct _StgEntCounter {
  /* Using StgWord for everything, becuase both the C and asm code
     generators make trouble if you try to pack things tighter */
    StgWord	registeredp;	/* 0 == no, 1 == yes */
    StgInt	arity;		/* arity (static info) */
    StgInt	stk_args;	/* # of args off stack */
				/* (rest of args are in registers) */
    char   	*str;		/* name of the thing */
    char   	*arg_kinds;	/* info about the args types */
    StgInt	entry_count;	/* Trips to fast entry code */
    StgInt      allocs;         /* number of allocations by this fun */
    struct _StgEntCounter *link;/* link to chain them all together */
} StgEntCounter;


#endif /* RTS_H */
