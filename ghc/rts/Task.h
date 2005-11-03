/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2001-2005
 *
 * Tasks
 *
 * -------------------------------------------------------------------------*/

#ifndef TASK_H
#define TASK_H

#include "GetTime.h"

/* 
   Definition of a Task
   --------------------
 
   A task is an OSThread that runs Haskell code.  Every OSThread
   created by the RTS for the purposes of running Haskell code is a
   Task, and OS threads that enter the Haskell RTS for the purposes of
   making a call-in are also Tasks.
   
   The relationship between the number of tasks and capabilities, and
   the runtime build (-threaded, -smp etc.) is summarised by the
   following table:

     build        Tasks   Capabilities
     ---------------------------------
     normal         1          1
     -threaded      N          1
     -smp           N          N

   The non-threaded build has a single Task and a single global
   Capability.
   
   The 'threaded' build has multiple Tasks, but a single Capability.
   At any one time only one task executing STG code, other tasks are
   either busy executing code outside the RTS (e.g., a C call) or
   waiting for their turn to (again) evaluate some STG code. A task
   relinquishes its RTS token when it is asked to evaluate an external
   (C) call.
   
   The SMP build allows multiple tasks and mulitple Capabilities.
   Multiple Tasks may all be running Haskell code simultaneously.

   In general, there may be multiple Tasks for an OS thread.  This
   happens if one Task makes a foreign call from Haskell, and
   subsequently calls back in to create a new bound thread.

   A particular Task structure can belong to more than one OS thread
   over its lifetime.  This is to avoid creating an unbounded number
   of Task structures.  The stats just accumulate.

   Ownership of Task
   -----------------

   The OS thread named in the Task structure has exclusive access to
   the structure, as long as it is the running_task of its Capability.
   That is, if (task->cap->running_task == task), then task->id owns
   the Task.  Otherwise the Task is owned by the owner of the parent
   data structure on which it is sleeping; for example, if the task is
   sleeping on spare_workers field of a Capability, then the owner of the
   Capability has access to the Task.

   When a task is migrated from sleeping on one Capability to another,
   its task->cap field must be modified.  When the task wakes up, it
   will read the new value of task->cap to find out which Capability
   it belongs to.  Hence some synchronisation is required on
   task->cap, and this is why we have task->lock.

   If the Task is not currently owned by task->id, then the thread is
   either

      (a) waiting on the condition task->cond.  The Task is either
         (1) a bound Task, the TSO will be on a queue somewhere
	 (2) a worker task, on the spare_workers queue of task->cap.

     (b) making a foreign call.  The Task will be on the
         suspended_ccalling_tasks list.

   We re-establish ownership in each case by respectively

      (a) the task is currently blocked in yieldCapability().
          This call will return when we have ownership of the Task and
          a Capability.  The Capability we get might not be the same
	  as the one we had when we called yieldCapability().
          
      (b) we must call resumeThread(task), which will safely establish
          ownership of the Task and a Capability.
*/

typedef struct Task_ {
#if defined(THREADED_RTS)
    OSThreadId id;		// The OS Thread ID of this task
#endif

    // This points to the Capability that the Task "belongs" to.  If
    // the Task owns a Capability, then task->cap points to it.  If
    // the task does not own a Capability, then either (a) if the task
    // is a worker, then task->cap points to the Capability it belongs
    // to, or (b) it is returning from a foreign call, then task->cap
    // points to the Capability with the returning_worker queue that this
    // this Task is on.
    //
    // When a task goes to sleep, it may be migrated to a different
    // Capability.  Hence, we always check task->cap on wakeup.  To
    // syncrhonise between the migrater and the migratee, task->lock
    // must be held when modifying task->cap.
    struct Capability_ *cap;

    rtsBool    stopped;         // this task has stopped or exited Haskell
    StgTSO *   suspended_tso;   // the TSO is stashed here when we
				// make a foreign call (NULL otherwise);

    // The following 3 fields are used by bound threads:
    StgTSO *   tso;             // the bound TSO (or NULL)
    SchedulerStatus  stat;      // return status
    StgClosure **    ret;       // return value

#if defined(THREADED_RTS)
    Condition cond;             // used for sleeping & waking up this task
    Mutex lock;			// lock for the condition variable

    // this flag tells the task whether it should wait on task->cond
    // or just continue immediately.  It's a workaround for the fact
    // that signalling a condition variable doesn't do anything if the
    // thread is already running, but we want it to be sticky.
    rtsBool wakeup;
#endif

    // Stats that we collect about this task
    // ToDo: we probably want to put this in a separate TaskStats
    // structure, so we can share it between multiple Tasks.  We don't
    // really want separate stats for each call in a nested chain of
    // foreign->haskell->foreign->haskell calls, but we'll get a
    // separate Task for each of the haskell calls.
    Ticks       elapsedtimestart;
    Ticks       muttimestart;
    Ticks       mut_time;
    Ticks       mut_etime;
    Ticks       gc_time;
    Ticks       gc_etime;

    // Links tasks onto various lists. (ToDo: do we need double
    // linking now?)
    struct Task_ *prev;
    struct Task_ *next;

    // Links tasks on the returning_tasks queue of a Capability.
    struct Task_ *return_link;

    // Links tasks on the all_tasks list
    struct Task_ *all_link;

    // When a Haskell thread makes a foreign call that re-enters
    // Haskell, we end up with another Task associated with the
    // current thread.  We have to remember the whole stack of Tasks
    // associated with the current thread so that we can correctly
    // save & restore the thread-local current task pointer.
    struct Task_ *prev_stack;
} Task;

INLINE_HEADER rtsBool
isBoundTask (Task *task) 
{
    return (task->tso != NULL);
}


// Linked list of all tasks.
//
extern Task *all_tasks;

// Start and stop the task manager.
// Requires: sched_mutex.
//
void initTaskManager (void);
void stopTaskManager (void);

// Create a new Task for a bound thread
// Requires: sched_mutex.
//
Task *newBoundTask (void);

// The current task is a bound task that is exiting.
// Requires: sched_mutex.
//
void boundTaskExiting (Task *task);

// This must be called when a new Task is associated with the current
// thread.  It sets up the thread-local current task pointer so that
// myTask() can work.
INLINE_HEADER void taskEnter (Task *task);

// Notify the task manager that a task has stopped.  This is used
// mainly for stats-gathering purposes.
// Requires: sched_mutex.
//
void taskStop (Task *task);

// Put the task back on the free list, mark it stopped.  Used by
// forkProcess().
//
void discardTask (Task *task);

// Get the Task associated with the current OS thread (or NULL if none).
//
INLINE_HEADER Task *myTask (void);

// After a fork, the tasks are not carried into the child process, so
// we must tell the task manager.
// Requires: sched_mutex.
//
void resetTaskManagerAfterFork (void);

#if defined(THREADED_RTS)

// Workers are attached to the supplied Capability.  This Capability
// should not currently have a running_task, because the new task
// will become the running_task for that Capability.
// Requires: sched_mutex.
//
void startWorkerTask  (struct Capability_ *cap, 
		       void OSThreadProcAttr (*taskStart)(Task *task));

#endif /* THREADED_RTS */

// -----------------------------------------------------------------------------
// INLINE functions... private from here on down:

// A thread-local-storage key that we can use to get access to the
// current thread's Task structure.
#if defined(THREADED_RTS)
extern ThreadLocalKey currentTaskKey;
#else
extern Task *my_task;
#endif

//
// myTask() uses thread-local storage to find the Task associated with
// the current OS thread.  If the current OS thread has multiple
// Tasks, because it has re-entered the RTS, then the task->prev_stack
// field is used to store the previous Task.
//
INLINE_HEADER Task *
myTask (void)
{
#if defined(THREADED_RTS)
    return getThreadLocalVar(&currentTaskKey);
#else
    return my_task;
#endif
}

INLINE_HEADER void
setMyTask (Task *task)
{
#if defined(THREADED_RTS)
    setThreadLocalVar(&currentTaskKey,task);
#else
    my_task = task;
#endif
}

// This must be called when a new Task is associated with the current
// thread.  It sets up the thread-local current task pointer so that
// myTask() can work.
INLINE_HEADER void
taskEnter (Task *task)
{
    // save the current value, just in case this Task has been created
    // as a result of re-entering the RTS (defaults to NULL):
    task->prev_stack = myTask();
    setMyTask(task);
}

#endif /* TASK_H */
