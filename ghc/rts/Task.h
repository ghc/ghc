/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2001-2005
 *
 * Tasks
 *
 * -------------------------------------------------------------------------*/

#ifndef __TASK_H__
#define __TASK_H__

/* Definition of a Task:
 *
 * A task is an OSThread that runs Haskell code.  Every OSThread
 * created by the RTS for the purposes of running Haskell code is a
 * Task.  We maintain information about Tasks mainly for the purposes
 * of stats gathering.
 *
 * There may exist OSThreads that run Haskell code, but which aren't
 * tasks (they don't have an associated TaskInfo structure).  This
 * happens when a thread makes an in-call to Haskell: we don't want to
 * create a Task for every in-call and register stats for all these
 * threads, so it is not therefore mandatory to have a Task for every
 * thread running Haskell code.
 *
 * The SMP build lets multiple tasks concurrently execute STG code,
 * all sharing vital internal RTS data structures in a controlled manner.
 *
 * The 'threaded' build has at any one time only one task executing STG
 * code, other tasks are either busy executing code outside the RTS
 * (e.g., a C call) or waiting for their turn to (again) evaluate some
 * STG code. A task relinquishes its RTS token when it is asked to
 * evaluate an external (C) call.
 */

#if defined(RTS_SUPPORTS_THREADS) /* to the end */
/* 
 * Tasks evaluate Haskell code; the TaskInfo structure collects together
 * misc metadata about a task.
 */
typedef struct _TaskInfo {
  OSThreadId id;
  rtsBool    is_worker;		/* rtsFalse <=> is a bound thread */
  rtsBool    stopped;           /* this task has stopped or exited Haskell */
  long       elapsedtimestart;
  long       muttimestart;
  long       mut_time;
  long       mut_etime;
  long       gc_time;
  long       gc_etime;
} TaskInfo;

extern TaskInfo *taskTable;
extern nat taskCount;

/*
 * Start and stop the task manager.
 * Requires: sched_mutex.
 */
extern void initTaskManager (void);
extern void stopTaskManager (void);

/*
 * Two ways to start tasks: either singly or in a batch
 * Requires: sched_mutex.
 */
extern rtsBool startTasks (nat num, void (*taskStart)(void));
extern rtsBool startTask  (void (*taskStart)(void));

/*
 * Notify the task manager that a task has stopped.  This is used
 * mainly for stats-gathering purposes.
 * Requires: sched_mutex.
 */
extern void taskStop (void);

/*
 * After a fork, the tasks are not carried into the child process, so
 * we must tell the task manager.
 * Requires: sched_mutex.
 */
extern void resetTaskManagerAfterFork (void);

/*
 * Tell the task manager that the current OS thread is now a task,
 * because it has entered Haskell as a bound thread.
 * Requires: sched_mutex.
 */
extern TaskInfo* threadIsTask (OSThreadId id);

/*
 * Get the TaskInfo structure corresponding to an OSThread.  Returns
 * NULL if the thread is not a task.
 * Requires: sched_mutex.
 */
extern TaskInfo* taskOfId (OSThreadId id);

/*
 * Decides whether to call startTask() or not, based on how many
 * workers are already running and waiting for work.  Returns
 * rtsTrue if a worker was created.
 * Requires: sched_mutex.
 */
extern rtsBool maybeStartNewWorker (void (*taskStart)(void));

#endif /* RTS_SUPPORTS_THREADS */
#endif /* __TASK_H__ */
