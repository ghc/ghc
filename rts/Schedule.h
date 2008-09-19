/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2005
 *
 * Prototypes for functions in Schedule.c 
 * (RTS internal scheduler interface)
 *
 * -------------------------------------------------------------------------*/

#ifndef SCHEDULE_H
#define SCHEDULE_H

#include "OSThreads.h"
#include "Capability.h"

/* initScheduler(), exitScheduler()
 * Called from STG :  no
 * Locks assumed   :  none
 */
void initScheduler (void);
void exitScheduler (rtsBool wait_foreign);
void freeScheduler (void);

// Place a new thread on the run queue of the current Capability
void scheduleThread (Capability *cap, StgTSO *tso);

// Place a new thread on the run queue of a specified Capability
// (cap is the currently owned Capability, cpu is the number of
// the desired Capability).
void scheduleThreadOn(Capability *cap, StgWord cpu, StgTSO *tso);

/* awakenBlockedQueue()
 *
 * Takes a pointer to the beginning of a blocked TSO queue, and
 * wakes up the entire queue.
 * Called from STG :  yes
 * Locks assumed   :  none
 */
#if defined(GRAN)
void awakenBlockedQueue(StgBlockingQueueElement *q, StgClosure *node);
#elif defined(PAR)
void awakenBlockedQueue(StgBlockingQueueElement *q, StgClosure *node);
#else
void awakenBlockedQueue (Capability *cap, StgTSO *tso);
#endif

/* wakeUpRts()
 * 
 * Causes an OS thread to wake up and run the scheduler, if necessary.
 */
void wakeUpRts(void);

/* unblockOne()
 *
 * Put the specified thread on the run queue of the given Capability.
 * Called from STG :  yes
 * Locks assumed   :  we own the Capability.
 */
StgTSO * unblockOne (Capability *cap, StgTSO *tso);

/* raiseExceptionHelper */
StgWord raiseExceptionHelper (StgRegTable *reg, StgTSO *tso, StgClosure *exception);

/* findRetryFrameHelper */
StgWord findRetryFrameHelper (StgTSO *tso);

/* workerStart()
 * 
 * Entry point for a new worker task.
 * Called from STG :  NO
 * Locks assumed   :  none
 */
#if defined(THREADED_RTS)
void OSThreadProcAttr workerStart(Task *task);
#endif

#if defined(GRAN)
void    awaken_blocked_queue(StgBlockingQueueElement *q, StgClosure *node);
void    unlink_from_bq(StgTSO* tso, StgClosure* node);
void    initThread(StgTSO *tso, nat stack_size, StgInt pri);
#elif defined(PAR)
nat     run_queue_len(void);
void    awaken_blocked_queue(StgBlockingQueueElement *q, StgClosure *node);
void    initThread(StgTSO *tso, nat stack_size);
#else
char   *info_type(StgClosure *closure);    // dummy
char   *info_type_by_ip(StgInfoTable *ip); // dummy
void    awaken_blocked_queue(StgTSO *q);
void    initThread(StgTSO *tso, nat stack_size);
#endif

/* The state of the scheduler.  This is used to control the sequence
 * of events during shutdown, and when the runtime is interrupted
 * using ^C.
 */
#define SCHED_RUNNING       0  /* running as normal */
#define SCHED_INTERRUPTING  1  /* ^C detected, before threads are deleted */
#define SCHED_SHUTTING_DOWN 2  /* final shutdown */

extern rtsBool RTS_VAR(sched_state);

/* 
 * flag that tracks whether we have done any execution in this time slice.
 */
#define ACTIVITY_YES      0 /* there has been activity in the current slice */
#define ACTIVITY_MAYBE_NO 1 /* no activity in the current slice */
#define ACTIVITY_INACTIVE 2 /* a complete slice has passed with no activity */
#define ACTIVITY_DONE_GC  3 /* like 2, but we've done a GC too */

/* Recent activity flag.
 * Locks required  : Transition from MAYBE_NO to INACTIVE
 * happens in the timer signal, so it is atomic.  Trnasition from
 * INACTIVE to DONE_GC happens under sched_mutex.  No lock required
 * to set it to ACTIVITY_YES.
 */
extern nat recent_activity;

/* Thread queues.
 * Locks required  : sched_mutex
 *
 * In GranSim we have one run/blocked_queue per PE.
 */
#if defined(GRAN)
// run_queue_hds defined in GranSim.h
#else
extern  StgTSO *RTS_VAR(blackhole_queue);
#if !defined(THREADED_RTS)
extern  StgTSO *RTS_VAR(blocked_queue_hd), *RTS_VAR(blocked_queue_tl);
extern  StgTSO *RTS_VAR(sleeping_queue);
#endif
#endif

/* Set to rtsTrue if there are threads on the blackhole_queue, and
 * it is possible that one or more of them may be available to run.
 * This flag is set to rtsFalse after we've checked the queue, and
 * set to rtsTrue just before we run some Haskell code.  It is used
 * to decide whether we should yield the Capability or not.
 * Locks required  : none (see scheduleCheckBlackHoles()).
 */
extern rtsBool blackholes_need_checking;

#if defined(THREADED_RTS)
extern Mutex RTS_VAR(sched_mutex);
#endif

SchedulerStatus rts_mainLazyIO(HaskellObj p, /*out*/HaskellObj *ret);

/* Called by shutdown_handler(). */
void interruptStgRts (void);

nat  run_queue_len (void);

void resurrectThreads (StgTSO *);
void performPendingThrowTos (StgTSO *);

void printAllThreads(void);

/* debugging only 
 */
#ifdef DEBUG
void print_bq (StgClosure *node);
#endif
#if defined(PAR)
void print_bqe (StgBlockingQueueElement *bqe);
#endif

/* -----------------------------------------------------------------------------
 * Some convenient macros/inline functions...
 */

#if !IN_STG_CODE

/* END_TSO_QUEUE and friends now defined in includes/StgMiscClosures.h */

/* Add a thread to the end of the run queue.
 * NOTE: tso->link should be END_TSO_QUEUE before calling this macro.
 * ASSUMES: cap->running_task is the current task.
 */
INLINE_HEADER void
appendToRunQueue (Capability *cap, StgTSO *tso)
{
    ASSERT(tso->_link == END_TSO_QUEUE);
    if (cap->run_queue_hd == END_TSO_QUEUE) {
	cap->run_queue_hd = tso;
    } else {
	setTSOLink(cap, cap->run_queue_tl, tso);
    }
    cap->run_queue_tl = tso;
}

/* Push a thread on the beginning of the run queue.  Used for
 * newly awakened threads, so they get run as soon as possible.
 * ASSUMES: cap->running_task is the current task.
 */
INLINE_HEADER void
pushOnRunQueue (Capability *cap, StgTSO *tso)
{
    setTSOLink(cap, tso, cap->run_queue_hd);
    cap->run_queue_hd = tso;
    if (cap->run_queue_tl == END_TSO_QUEUE) {
	cap->run_queue_tl = tso;
    }
}

/* Pop the first thread off the runnable queue.
 */
INLINE_HEADER StgTSO *
popRunQueue (Capability *cap)
{ 
    StgTSO *t = cap->run_queue_hd;
    ASSERT(t != END_TSO_QUEUE);
    cap->run_queue_hd = t->_link;
    t->_link = END_TSO_QUEUE; // no write barrier req'd
    if (cap->run_queue_hd == END_TSO_QUEUE) {
	cap->run_queue_tl = END_TSO_QUEUE;
    }
    return t;
}

/* Add a thread to the end of the blocked queue.
 */
#if !defined(THREADED_RTS)
INLINE_HEADER void
appendToBlockedQueue(StgTSO *tso)
{
    ASSERT(tso->_link == END_TSO_QUEUE);
    if (blocked_queue_hd == END_TSO_QUEUE) {
	blocked_queue_hd = tso;
    } else {
	setTSOLink(&MainCapability, blocked_queue_tl, tso);
    }
    blocked_queue_tl = tso;
}
#endif

#if defined(THREADED_RTS)
// Assumes: my_cap is owned by the current Task.  We hold
// other_cap->lock, but we do not necessarily own other_cap; another
// Task may be running on it.
INLINE_HEADER void
appendToWakeupQueue (Capability *my_cap, Capability *other_cap, StgTSO *tso)
{
    ASSERT(tso->_link == END_TSO_QUEUE);
    if (other_cap->wakeup_queue_hd == END_TSO_QUEUE) {
	other_cap->wakeup_queue_hd = tso;
    } else {
        // my_cap is passed to setTSOLink() because it may need to
        // write to the mutable list.
	setTSOLink(my_cap, other_cap->wakeup_queue_tl, tso);
    }
    other_cap->wakeup_queue_tl = tso;
}
#endif

/* Check whether various thread queues are empty
 */
INLINE_HEADER rtsBool
emptyQueue (StgTSO *q)
{
    return (q == END_TSO_QUEUE);
}

INLINE_HEADER rtsBool
emptyRunQueue(Capability *cap)
{
    return emptyQueue(cap->run_queue_hd);
}

#if defined(THREADED_RTS)
INLINE_HEADER rtsBool
emptyWakeupQueue(Capability *cap)
{
    return emptyQueue(cap->wakeup_queue_hd);
}
#endif

#if !defined(THREADED_RTS)
#define EMPTY_BLOCKED_QUEUE()  (emptyQueue(blocked_queue_hd))
#define EMPTY_SLEEPING_QUEUE() (emptyQueue(sleeping_queue))
#endif

INLINE_HEADER rtsBool
emptyThreadQueues(Capability *cap)
{
    return emptyRunQueue(cap)
#if !defined(THREADED_RTS)
	&& EMPTY_BLOCKED_QUEUE() && EMPTY_SLEEPING_QUEUE()
#endif
    ;
}

#endif /* !IN_STG_CODE */

#endif /* SCHEDULE_H */

