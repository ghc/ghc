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

void setThreadFlag       (Capability *from, StgTSO *tso, StgWord32 flag);
void unsetThreadFlag     (Capability *from, StgTSO *tso, StgWord32 flag);
void updThreadFlag       (Capability *from, StgTSO *tso, StgWord32 flag, StgBool set);

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

// Word offsets from the start of an stg_ctoi_t frame; they count the
// frame header, which has FRAME_HDR_W words (two under two-word frames).
#define CTOI_OLD_TUPLE_SPILL_WORDS_OFFSET (3 + FRAME_HDR_W)
#define CTOI_TUPLE_INFO_OFFSET (1 + FRAME_HDR_W)
void restoreStackInvariants(StgTSO *tso, StgPtr sp, StgWord words);

#if defined(DEBUG)
void printThreadBlockage (StgTSO *tso);
void printThreadStatus (StgTSO *t);
void printAllThreads (void);
void printGlobalThreads(void);
void printThreadQueue (StgTSO *t);
#endif

#include "EndPrivate.h"
