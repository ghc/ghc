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
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_THREADS_H
#define RTS_THREADS_H

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

//
// Creating threads
//
StgTSO *createThread (Capability *cap, W_ stack_size);

void scheduleWaitThread (/* in    */ StgTSO *tso,
                         /* out   */ HaskellObj* ret,
                         /* inout */ Capability **cap,
                         /* in    */ rtsBool skipAppend);

void pushClosure (StgTSO *tso, StgWord c);

StgTSO *createGenThread       (Capability *cap, W_ stack_size,
			       StgClosure *closure);
StgTSO *createIOThread        (Capability *cap, W_ stack_size,
			       StgClosure *closure);
StgTSO *createUserLevelThread (Capability *cap, W_ stack_size,
			       StgClosure *closure);
StgTSO *createStrictIOThread  (Capability *cap, W_ stack_size,
			       StgClosure *closure);

void setOwningCapability (Capability* cap, StgTSO* tso, nat target);

// Suspending/resuming threads around foreign calls
void *        suspendThread (StgRegTable *, rtsBool interruptible);
StgRegTable * resumeThread  (void *);

//
// Thread operations from Threads.c
//
int    cmp_thread      (StgPtr tso1, StgPtr tso2);
int    rts_getThreadId (StgPtr tso);
void   labelThread          (Capability *cap,
                             StgTSO     *tso,
                             char       *label);


#if !defined(mingw32_HOST_OS)
pid_t  forkProcess     (HsStablePtr *entry);
#else
pid_t  forkProcess     (HsStablePtr *entry)
    GNU_ATTRIBUTE(__noreturn__);
#endif

HsBool rtsSupportsBoundThreads (void);

// The number of Capabilities
extern unsigned int n_capabilities;

// The number of Capabilities that are not disabled
extern nat enabled_capabilities;

#if !IN_STG_CODE
extern Capability MainCapability;
#endif

//
// Change the number of capabilities (only supports increasing the
// current value at the moment).
//
extern void setNumCapabilities (nat new);

#endif /* RTS_THREADS_H */
