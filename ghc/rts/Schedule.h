/* -----------------------------------------------------------------------------
 * $Id: Schedule.h,v 1.10 1999/11/09 15:46:55 simonmar Exp $
 *
 * (c) The GHC Team 1998-1999
 *
 * Prototypes for functions in Schedule.c 
 * (RTS internal scheduler interface)
 *
 * ---------------------------------------------------------------------------*/

/* initScheduler(), exitScheduler(), startTasks()
 * 
 * Called from STG :  no
 * Locks assumed   :  none
 */
void initScheduler( void );
void exitScheduler( void );
#ifdef SMP
void startTasks( void );
#endif

/* awakenBlockedQueue()
 *
 * Takes a pointer to the beginning of a blocked TSO queue, and
 * wakes up the entire queue.
 *
 * Called from STG :  yes
 * Locks assumed   :  none
 */
void awakenBlockedQueue(StgTSO *tso);

/* unblockOne()
 *
 * Takes a pointer to the beginning of a blocked TSO queue, and
 * removes the first thread, placing it on the runnable queue.
 *
 * Called from STG : yes
 * Locks assumed   : none
 */
StgTSO *unblockOne(StgTSO *tso);

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

/* Context switch flag.
 * Locks required  : sched_mutex
 */
extern nat context_switch;

extern  nat ticks_since_select;

/* Capability type
 */
typedef StgRegTable Capability;

/* Free capability list.
 * Locks required: sched_mutex.
 */
#ifdef SMP
extern Capability *free_capabilities;
extern nat n_free_capabilities;
#else
extern Capability MainRegTable;
#endif

/* Thread queues.
 * Locks required  : sched_mutex
 */
extern  StgTSO *run_queue_hd, *run_queue_tl;
extern  StgTSO *blocked_queue_hd, *blocked_queue_tl;

#ifdef DEBUG
extern void printThreadBlockage(StgTSO *tso);
#endif

#ifdef SMP
extern pthread_mutex_t sched_mutex;
extern pthread_cond_t  thread_ready_cond;
extern pthread_cond_t  gc_pending_cond;
#endif

#ifdef SMP
typedef struct {
  pthread_t id;
  double    elapsedtimestart;
  double    mut_time;
  double    mut_etime;
  double    gc_time;
  double    gc_etime;
} task_info;

extern task_info *task_ids;
#endif

/* -----------------------------------------------------------------------------
 * Some convenient macros...
 */

#define END_TSO_QUEUE  ((StgTSO *)(void*)&END_TSO_QUEUE_closure)
#define END_CAF_LIST   ((StgCAF *)(void*)&END_TSO_QUEUE_closure)

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
#ifdef SMP
#define THREAD_RUNNABLE()			\
  if (free_capabilities != NULL) {		\
     pthread_cond_signal(&thread_ready_cond);	\
  }						\
  context_switch = 1;
#else
#define THREAD_RUNNABLE()  /* nothing */
#endif

