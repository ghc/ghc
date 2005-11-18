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
void exitScheduler (void);

// Place a new thread on the run queue of the specified Capability
void scheduleThread (Capability *cap, StgTSO *tso);

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

/* unblockOne()
 *
 * Put the specified thread on the run queue of the given Capability.
 * Called from STG :  yes
 * Locks assumed   :  we own the Capability.
 */
StgTSO * unblockOne(Capability *cap, StgTSO *tso);

/* raiseAsync()
 *
 * Raises an exception asynchronously in the specified thread.
 *
 * Called from STG :  yes
 * Locks assumed   :  none
 */
void raiseAsync(Capability *cap, StgTSO *tso, StgClosure *exception);

/* suspendComputation()
 *
 * A variant of raiseAsync(), this strips the stack of the specified
 * thread down to the stop_here point, leaving a current closure on
 * top of the stack at [stop_here - 1].
 */
void suspendComputation(Capability *cap, StgTSO *tso, StgPtr stop_here);

/* raiseExceptionHelper */
StgWord raiseExceptionHelper (StgRegTable *reg, StgTSO *tso, StgClosure *exception);

/* findRetryFrameHelper */
StgWord findRetryFrameHelper (StgTSO *tso);

/* GetRoots(evac_fn f)
 *
 * Call f() for each root known to the scheduler.
 *
 * Called from STG :  NO
 * Locks assumed   :  ????
 */
void GetRoots(evac_fn);

/* workerStart()
 * 
 * Entry point for a new worker task.
 * Called from STG :  NO
 * Locks assumed   :  none
 */
void workerStart(Task *task);

// ToDo: check whether all fcts below are used in the SMP version, too
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

/* Context switch flag.
 * Locks required  : none (conflicts are harmless)
 */
extern int RTS_VAR(context_switch);

/* Interrupted flag.
 * Locks required  : none (makes one transition from false->true)
 */
extern rtsBool RTS_VAR(interrupted);

/* Shutdown flag.
 * Locks required  : none (makes one transition from false->true)
 */
extern rtsBool shutting_down_scheduler;

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

/* Linked list of all threads.
 * Locks required  : sched_mutex
 */
extern  StgTSO *RTS_VAR(all_threads);

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

StgBool isThreadBound(StgTSO *tso);

SchedulerStatus rts_mainLazyIO(HaskellObj p, /*out*/HaskellObj *ret);

/* Called by shutdown_handler(). */
void interruptStgRts (void);

nat  run_queue_len (void);

void resurrectThreads (StgTSO *);

void printAllThreads(void);

/* debugging only 
 */
#ifdef DEBUG
void print_bq (StgClosure *node);
#endif
#if defined(PAR)
void print_bqe (StgBlockingQueueElement *bqe);
#endif

void labelThread(StgPtr tso, char *label);

/* -----------------------------------------------------------------------------
 * Some convenient macros/inline functions...
 */

#if !IN_STG_CODE

/* END_TSO_QUEUE and friends now defined in includes/StgMiscClosures.h */

/* Add a thread to the end of the run queue.
 * NOTE: tso->link should be END_TSO_QUEUE before calling this macro.
 * ASSUMES: cap->running_task is the current task.
 */
STATIC_INLINE void
appendToRunQueue (Capability *cap, StgTSO *tso)
{
    ASSERT(tso->link == END_TSO_QUEUE);
    if (cap->run_queue_hd == END_TSO_QUEUE) {
	cap->run_queue_hd = tso;
    } else {
	cap->run_queue_tl->link = tso;
    }
    cap->run_queue_tl = tso;
}

/* Push a thread on the beginning of the run queue.  Used for
 * newly awakened threads, so they get run as soon as possible.
 * ASSUMES: cap->running_task is the current task.
 */
STATIC_INLINE void
pushOnRunQueue (Capability *cap, StgTSO *tso)
{
    tso->link = cap->run_queue_hd;
    cap->run_queue_hd = tso;
    if (cap->run_queue_tl == END_TSO_QUEUE) {
	cap->run_queue_tl = tso;
    }
}

/* Pop the first thread off the runnable queue.
 */
STATIC_INLINE StgTSO *
popRunQueue (Capability *cap)
{ 
    StgTSO *t = cap->run_queue_hd;
    ASSERT(t != END_TSO_QUEUE);
    cap->run_queue_hd = t->link;
    t->link = END_TSO_QUEUE;
    if (cap->run_queue_hd == END_TSO_QUEUE) {
	cap->run_queue_tl = END_TSO_QUEUE;
    }
    return t;
}

/* Add a thread to the end of the blocked queue.
 */
#if !defined(THREADED_RTS)
STATIC_INLINE void
appendToBlockedQueue(StgTSO *tso)
{
    ASSERT(tso->link == END_TSO_QUEUE);
    if (blocked_queue_hd == END_TSO_QUEUE) {
	blocked_queue_hd = tso;
    } else {
	blocked_queue_tl->link = tso;
    }
    blocked_queue_tl = tso;
}
#endif

/* Check whether various thread queues are empty
 */
STATIC_INLINE rtsBool
emptyQueue (StgTSO *q)
{
    return (q == END_TSO_QUEUE);
}

STATIC_INLINE rtsBool
emptyRunQueue(Capability *cap)
{
    return emptyQueue(cap->run_queue_hd);
}

#if !defined(THREADED_RTS)
#define EMPTY_BLOCKED_QUEUE()  (emptyQueue(blocked_queue_hd))
#define EMPTY_SLEEPING_QUEUE() (emptyQueue(sleeping_queue))
#endif

STATIC_INLINE rtsBool
emptyThreadQueues(Capability *cap)
{
    return emptyRunQueue(cap)
#if !defined(THREADED_RTS)
	&& EMPTY_BLOCKED_QUEUE() && EMPTY_SLEEPING_QUEUE()
#endif
    ;
}

#ifdef DEBUG
void sched_belch(char *s, ...)
   GNU_ATTRIBUTE(format (printf, 1, 2));
#endif

#endif /* !IN_STG_CODE */

#endif /* SCHEDULE_H */

