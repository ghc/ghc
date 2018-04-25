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
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif

//
// Creating threads
//
StgTSO *createThread (Capability *cap, W_ stack_size);

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
void *        suspendThread (StgRegTable *, bool interruptible);
StgRegTable * resumeThread  (void *);

//
// Thread operations from Threads.c
//
int     cmp_thread                       (StgPtr tso1, StgPtr tso2);
int     rts_getThreadId                  (StgPtr tso);
HsInt64 rts_getThreadAllocationCounter   (StgPtr tso);
void    rts_setThreadAllocationCounter   (StgPtr tso, HsInt64 i);
void    rts_enableThreadAllocationLimit  (StgPtr tso);
void    rts_disableThreadAllocationLimit (StgPtr tso);

#if !defined(mingw32_HOST_OS)
pid_t  forkProcess     (HsStablePtr *entry);
#else
pid_t  forkProcess     (HsStablePtr *entry)
    GNU_ATTRIBUTE(__noreturn__);
#endif

HsBool rtsSupportsBoundThreads (void);

// The number of Capabilities.
// ToDo: I would like this to be private to the RTS and instead expose a
// function getNumCapabilities(), but it is used in compiler/cbits/genSym.c
extern unsigned int n_capabilities;

// The number of Capabilities that are not disabled
extern uint32_t enabled_capabilities;

#if !IN_STG_CODE
extern Capability MainCapability;
#endif

//
// Change the number of capabilities (only supports increasing the
// current value at the moment).
//
extern void setNumCapabilities (uint32_t new_);
