/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006
 *
 * Thread-related functionality
 *
 * --------------------------------------------------------------------------*/

#ifndef THREADS_H
#define THREADS_H

#include "BeginPrivate.h"

#define END_BLOCKED_EXCEPTIONS_QUEUE ((MessageThrowTo*)END_TSO_QUEUE)

StgTSO * unblockOne (Capability *cap, StgTSO *tso);
StgTSO * unblockOne_ (Capability *cap, StgTSO *tso, rtsBool allow_migrate);

void checkBlockingQueues (Capability *cap, StgTSO *tso);
void wakeBlockingQueue   (Capability *cap, StgBlockingQueue *bq);
void tryWakeupThread     (Capability *cap, StgTSO *tso);
void migrateThread       (Capability *from, StgTSO *tso, Capability *to);
void pushCallToClosure   (Capability *cap, StgTSO *tso, StgClosure* closure);

rtsBool hasHaskellScheduler (StgTSO* tso);
rtsBool isThreadSleeping (StgTSO* tso);


// Wakes up a thread on a Capability (probably a different Capability
// from the one held by the current Task).
//
#ifdef THREADED_RTS
void wakeupThreadOnCapability (Capability *cap,
                               Capability *other_cap,
                               StgTSO *tso);
#endif

void updateThunk         (Capability *cap, StgTSO *tso,
                          StgClosure *thunk, StgClosure *val);

rtsBool removeThreadFromQueue     (Capability *cap, StgTSO **queue, StgTSO *tso);
rtsBool removeThreadFromDeQueue   (Capability *cap, StgTSO **head, StgTSO **tail, StgTSO *tso);

StgBool isThreadBound (StgTSO* tso);

// Overfow/underflow
void threadStackOverflow  (Capability *cap, StgTSO *tso);
W_   threadStackUnderflow (Capability *cap, StgTSO *tso);

#ifdef DEBUG
void printThreadBlockage (StgTSO *tso);
void printThreadStatus (StgTSO *t);
void printAllThreads (void);
void printThreadQueue (StgTSO *t);
void printStackFrames (StgTSO *t);
#endif

#include "EndPrivate.h"

#endif /* THREADS_H */
