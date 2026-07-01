/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2009
 *
 * External API for the scheduler.  For most uses, the functions in
 * RtsAPI.h should be enough.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#ifndef RTS_EXPORT
# define RTS_EXPORT
#endif

#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif

#include "rts/storage/TSO.h"

//
// Creating threads
//
RTS_EXPORT StgTSO *createThread (Capability *cap, W_ stack_size);

// precondition:
//   (*cap)->running_task != NULL
//   (*cap)->running_task must be a bound task (e.g. newBoundTask() has been
//                        called on that thread).
RTS_EXPORT void scheduleWaitThread (/* in    */ StgTSO *tso,
                         /* out   */ HaskellObj* ret,
                         /* inout */ Capability **cap);

RTS_EXPORT StgTSO *createGenThread       (Capability *cap, W_ stack_size,
                               StgClosure *closure);
RTS_EXPORT StgTSO *createIOThread        (Capability *cap, W_ stack_size,
                               StgClosure *closure);
RTS_EXPORT StgTSO *createStrictIOThread  (Capability *cap, W_ stack_size,
                               StgClosure *closure);

// Suspending/resuming threads around foreign calls
RTS_EXPORT void *        suspendThread (StgRegTable *, bool interruptible);
RTS_EXPORT StgRegTable * resumeThread  (void *);

//
// Thread operations from Threads.c
//
RTS_EXPORT bool        eq_thread                        (StgPtr tso1, StgPtr tso2);
RTS_EXPORT int         cmp_thread                       (StgPtr tso1, StgPtr tso2);
RTS_EXPORT StgThreadID rts_getThreadId                  (StgPtr tso);
RTS_EXPORT void        rts_enableThreadAllocationLimit  (StgPtr tso);
RTS_EXPORT void        rts_disableThreadAllocationLimit (StgPtr tso);

// Forward declarations, defined in Closures.h
struct _StgMutArrPtrs;
RTS_EXPORT struct _StgMutArrPtrs *listThreads               (Capability *cap);

#if !defined(mingw32_HOST_OS)
RTS_EXPORT pid_t  forkProcess     (HsStablePtr *entry);
#else
RTS_EXPORT pid_t  forkProcess     (HsStablePtr *entry)
    STG_NORETURN;
#endif

RTS_EXPORT HsBool rtsSupportsBoundThreads (void);

// The number of Capabilities.
// TODO: Ideally we would only provide getNumCapabilities
// but this is used in compiler/cbits/genSym.c
extern RTS_EXPORT uint32_t n_capabilities;

INLINE_HEADER unsigned int getNumCapabilities(void)
{ return RELAXED_LOAD(&n_capabilities); }

// The number of Capabilities that are not disabled
extern RTS_EXPORT uint32_t enabled_capabilities;

// The maximum number of Capabilities supported by the RTS.
// See Note [Capabilities array sizing] in rts/Capability.c.
extern RTS_EXPORT uint32_t max_n_capabilities;

#if !IN_STG_CODE
extern RTS_EXPORT Capability MainCapability;
#endif

//
// Change the number of capabilities (only supports increasing the
// current value at the moment).
//
extern RTS_EXPORT void setNumCapabilities (uint32_t new_);

RTS_EXPORT bool performTryPutMVar(Capability *cap, StgMVar *mvar, StgClosure *value);
