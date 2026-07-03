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

#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif

#include "rts/storage/TSO.h"

//
// Creating threads
//
StgTSO *createThread (Capability *cap, W_ stack_size);

// precondition:
//   (*cap)->running_task != NULL
//   (*cap)->running_task must be a bound task (e.g. newBoundTask() has been
//                        called on that thread).
void scheduleWaitThread (/* in    */ StgTSO *tso,
                         /* out   */ HaskellObj* ret,
                         /* inout */ Capability **cap);

StgTSO *createGenThread       (Capability *cap, W_ stack_size,
                               StgClosure *closure);
StgTSO *createIOThread        (Capability *cap, W_ stack_size,
                               StgClosure *closure);
StgTSO *createStrictIOThread  (Capability *cap, W_ stack_size,
                               StgClosure *closure);

// Suspending/resuming threads around foreign calls
RTS_PUBLIC void *        suspendThread (StgRegTable *, bool interruptible);
RTS_PUBLIC StgRegTable * resumeThread  (void *);

//
// Thread operations from Threads.c
//
RTS_PUBLIC bool        eq_thread                        (StgPtr tso1, StgPtr tso2);
RTS_PUBLIC int         cmp_thread                       (StgPtr tso1, StgPtr tso2);
RTS_PUBLIC StgThreadID rts_getThreadId                  (StgPtr tso);
RTS_PUBLIC void        rts_enableThreadAllocationLimit  (StgPtr tso);
RTS_PUBLIC void        rts_disableThreadAllocationLimit (StgPtr tso);

// Forward declarations, defined in Closures.h
struct _StgMutArrPtrs;
struct _StgMutArrPtrs *listThreads               (Capability *cap);

#if !defined(mingw32_HOST_OS)
RTS_PUBLIC pid_t  forkProcess     (HsStablePtr *entry);
#else
RTS_PUBLIC pid_t  forkProcess     (HsStablePtr *entry)
    STG_NORETURN;
#endif

RTS_PUBLIC HsBool rtsSupportsBoundThreads (void);

// The number of Capabilities.
// TODO: Ideally we would only provide getNumCapabilities
// but this is used in compiler/cbits/genSym.c
extern RTS_PUBLIC uint32_t n_capabilities;

INLINE_HEADER unsigned int getNumCapabilities(void)
{ return RELAXED_LOAD(&n_capabilities); }

// The number of Capabilities that are not disabled
extern RTS_PUBLIC uint32_t enabled_capabilities;

// The maximum number of Capabilities supported by the RTS.
// See Note [Capabilities array sizing] in rts/Capability.c.
extern RTS_PUBLIC uint32_t max_n_capabilities;

#if !IN_STG_CODE
extern RTS_PUBLIC Capability MainCapability;
#endif

//
// Change the number of capabilities (only supports increasing the
// current value at the moment).
//
extern RTS_PUBLIC void setNumCapabilities (uint32_t new_);

RTS_PUBLIC bool performTryPutMVar(Capability *cap, StgMVar *mvar, StgClosure *value);
