/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006
 *
 * Thread-related functionality
 *
 * --------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

#define END_BLOCKED_EXCEPTIONS_QUEUE ((MessageThrowTo*)END_TSO_QUEUE)

StgTSO * unblockOne (Capability *cap, StgTSO *tso);
StgTSO * unblockOne_ (Capability *cap, StgTSO *tso, bool allow_migrate);

void checkBlockingQueues (Capability *cap, StgTSO *tso);
void tryWakeupThread     (Capability *cap, StgTSO *tso);
void migrateThread       (Capability *from, StgTSO *tso, Capability *to);

// Wakes up a thread on a Capability (probably a different Capability
// from the one held by the current Task).
//
#if defined(THREADED_RTS)
void wakeupThreadOnCapability (Capability *cap,
                               Capability *other_cap, 
                               StgTSO *tso);
#endif

void updateThunk         (Capability *cap, StgTSO *tso,
                          StgClosure *thunk, StgClosure *val);

bool removeThreadFromQueue     (Capability *cap, StgTSO **queue, StgTSO *tso);
bool removeThreadFromDeQueue   (Capability *cap, StgTSO **head, StgTSO **tail, StgTSO *tso);

StgBool isThreadBound (StgTSO* tso);

// Overflow/underflow
void threadStackOverflow  (Capability *cap, StgTSO *tso);
W_   threadStackUnderflow (Capability *cap, StgTSO *tso);

bool performTryPutMVar(Capability *cap, StgMVar *mvar, StgClosure *value);

#if defined(DEBUG)
void printThreadBlockage (StgTSO *tso);
void printThreadStatus (StgTSO *t);
void printAllThreads (void);
void printThreadQueue (StgTSO *t);
#endif

#include "EndPrivate.h"
