/* -----------------------------------------------------------------------------
 * $Id: Schedule.h,v 1.31 2002/03/12 13:57:12 simonmar Exp $
 *
 * (c) The GHC Team 1998-1999
 *
 * Prototypes for functions in Schedule.c 
 * (RTS internal scheduler interface)
 *
 * -------------------------------------------------------------------------*/

#ifndef __SCHEDULE_H__
#define __SCHEDULE_H__
#include "OSThreads.h"

/* initScheduler(), exitScheduler(), startTasks()
 * 
 * Called from STG :  no
 * Locks assumed   :  none
 */
extern void initScheduler  ( void );
extern void exitScheduler  ( void );

/* awakenBlockedQueue()
 *
 * Takes a pointer to the beginning of a blocked TSO queue, and
 * wakes up the entire queue.
 *
 * Called from STG :  yes
 * Locks assumed   :  none
 */
#if defined(GRAN)
void awakenBlockedQueue(StgBlockingQueueElement *q, StgClosure *node);
#elif defined(PAR)
void awakenBlockedQueue(StgBlockingQueueElement *q, StgClosure *node);
#else
void awakenBlockedQueue(StgTSO *tso);
#endif

/* unblockOne()
 *
 * Takes a pointer to the beginning of a blocked TSO queue, and
 * removes the first thread, placing it on the runnable queue.
 *
 * Called from STG : yes
 * Locks assumed   : none
 */
#if defined(GRAN) || defined(PAR)
StgBlockingQueueElement *unblockOne(StgBlockingQueueElement *bqe, StgClosure *node);
#else
StgTSO *unblockOne(StgTSO *tso);
#endif

/* raiseAsync()
 *
 * Raises an exception asynchronously in the specified thread.
 *
 * Called from STG :  yes
 * Locks assumed   :  none
 */
void raiseAsync(StgTSO *tso, StgClosure *exception);

/* awaitEvent()
 *
 * Raises an exception asynchronously in the specified thread.
 *
 * Called from STG :  NO
 * Locks assumed   :  sched_mutex
 */
void awaitEvent(rtsBool wait);  /* In Select.c */

/* wakeUpSleepingThreads(nat ticks)
 *
 * Wakes up any sleeping threads whose timers have expired.
 *
 * Called from STG :  NO
 * Locks assumed   :  sched_mutex
 */
rtsBool wakeUpSleepingThreads(nat);  /* In Select.c */

/* GetRoots(evac_fn f)
 *
 * Call f() for each root known to the scheduler.
 *
 * Called from STG :  NO
 * Locks assumed   :  ????
 */
void GetRoots(evac_fn);

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
 * Locks required  : sched_mutex
 */
extern nat context_switch;
extern rtsBool interrupted;

/* In Select.c */
extern nat timestamp;

/* Thread queues.
 * Locks required  : sched_mutex
 *
 * In GranSim we have one run/blocked_queue per PE.
 */
#if defined(GRAN)
// run_queue_hds defined in GranSim.h
#else
extern  StgTSO *run_queue_hd, *run_queue_tl;
extern  StgTSO *blocked_queue_hd, *blocked_queue_tl;
extern  StgTSO *sleeping_queue;
#endif
/* Linked list of all threads. */
extern  StgTSO *all_threads;

#if defined(RTS_SUPPORTS_THREADS)
/* Schedule.c has detailed info on what these do */
extern Mutex       sched_mutex;
extern Condition   thread_ready_cond;
extern Condition   returning_worker_cond;
extern nat         rts_n_waiting_workers;
extern nat         rts_n_waiting_tasks;
#endif


/* Sigh, RTS-internal versions of waitThread(), scheduleThread(), and
   rts_evalIO() for the use by main() only. ToDo: better. */
extern SchedulerStatus waitThread_(StgTSO *tso,
				   /*out*/StgClosure **ret
#if defined(THREADED_RTS)
				   , rtsBool blockWaiting
#endif
				   );
extern SchedulerStatus rts_mainEvalIO(HaskellObj p, /*out*/HaskellObj *ret);


/* Called by shutdown_handler(). */
void interruptStgRts ( void );

void raiseAsync(StgTSO *tso, StgClosure *exception);
nat  run_queue_len(void);

void resurrectThreads( StgTSO * );

/* Main threads:
 *
 * These are the threads which clients have requested that we run.  
 *
 * In a 'threaded' build, we might have several concurrent clients all
 * waiting for results, and each one will wait on a condition variable
 * until the result is available.
 *
 * In non-SMP, clients are strictly nested: the first client calls
 * into the RTS, which might call out again to C with a _ccall_GC, and
 * eventually re-enter the RTS.
 *
 * This is non-abstract at the moment because the garbage collector
 * treats pointers to TSOs from the main thread list as "weak" - these
 * pointers won't prevent a thread from receiving a BlockedOnDeadMVar
 * exception.
 *
 * Main threads information is kept in a linked list:
 */
typedef struct StgMainThread_ {
  StgTSO *         tso;
  SchedulerStatus  stat;
  StgClosure **    ret;
#if defined(RTS_SUPPORTS_THREADS)
  Condition        wakeup;
#endif
  struct StgMainThread_ *link;
} StgMainThread;

/* Main thread queue.
 * Locks required: sched_mutex.
 */
extern StgMainThread *main_threads;

/* debugging only 
 */
#ifdef DEBUG
void printThreadBlockage(StgTSO *tso);
void printThreadStatus(StgTSO *tso);
void printAllThreads(void);
#endif
void print_bq (StgClosure *node);
#if defined(PAR)
void print_bqe (StgBlockingQueueElement *bqe);
#endif

/* -----------------------------------------------------------------------------
 * Some convenient macros...
 */

/* END_TSO_QUEUE and friends now defined in includes/StgMiscClosures.h */

/* Add a thread to the end of the run queue.
 * NOTE: tso->link should be END_TSO_QUEUE before calling this macro.
 */
#define APPEND_TO_RUN_QUEUE(tso)		\
    ASSERT(tso->link == END_TSO_QUEUE);		\
    if (run_queue_hd == END_TSO_QUEUE) {	\
      run_queue_hd = tso;			\
    } else {					\
      run_queue_tl->link = tso;			\
    }						\
    run_queue_tl = tso;

/* Push a thread on the beginning of the run queue.  Used for
 * newly awakened threads, so they get run as soon as possible.
 */
#define PUSH_ON_RUN_QUEUE(tso)			\
    tso->link = run_queue_hd;			\
      run_queue_hd = tso;			\
    if (run_queue_tl == END_TSO_QUEUE) {	\
      run_queue_tl = tso;			\
    }

/* Pop the first thread off the runnable queue.
 */
#define POP_RUN_QUEUE()				\
  ({ StgTSO *t = run_queue_hd;			\
    if (t != END_TSO_QUEUE) {			\
      run_queue_hd = t->link;			\
      t->link = END_TSO_QUEUE;			\
      if (run_queue_hd == END_TSO_QUEUE) {	\
        run_queue_tl = END_TSO_QUEUE;		\
      }						\
    }						\
    t;						\
  })

/* Add a thread to the end of the blocked queue.
 */
#define APPEND_TO_BLOCKED_QUEUE(tso)		\
    ASSERT(tso->link == END_TSO_QUEUE);		\
    if (blocked_queue_hd == END_TSO_QUEUE) {    \
      blocked_queue_hd = tso;			\
    } else {					\
      blocked_queue_tl->link = tso;		\
    }						\
    blocked_queue_tl = tso;

/* Signal that a runnable thread has become available, in
 * case there are any waiting tasks to execute it.
 */
#if defined(RTS_SUPPORTS_THREADS)
#define THREAD_RUNNABLE()			\
  if ( !noCapabilities() ) {	                \
     signalCondition(&thread_ready_cond);	\
  }						\
  context_switch = 1;
#else
#define THREAD_RUNNABLE()  /* nothing */
#endif

/* Check whether various thread queues are empty
 */
#define EMPTY_QUEUE(q)         (q == END_TSO_QUEUE)

#define EMPTY_RUN_QUEUE()      (EMPTY_QUEUE(run_queue_hd))
#define EMPTY_BLOCKED_QUEUE()  (EMPTY_QUEUE(blocked_queue_hd))
#define EMPTY_SLEEPING_QUEUE() (EMPTY_QUEUE(sleeping_queue))

#define EMPTY_THREAD_QUEUES()  (EMPTY_RUN_QUEUE() && \
				EMPTY_BLOCKED_QUEUE() && \
				EMPTY_SLEEPING_QUEUE())

#endif /* __SCHEDULE_H__ */
