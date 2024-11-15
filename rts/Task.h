/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2001-2005
 *
 * Tasks
 *
 * For details on the high-level design, see
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/scheduler
 *
 * -------------------------------------------------------------------------*/

#pragma once

#include "GetTime.h"

#include "BeginPrivate.h"

/*
   Note [Definition of a Task]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~
   A task is an OSThread that runs Haskell code.  Every OSThread that
   runs inside the RTS, whether as a worker created by the RTS or via
   an in-call from C to Haskell, has an associated Task.  The first
   time an OS thread calls into Haskell it is allocated a Task, which
   remains until the RTS is shut down.

   There is a one-to-one relationship between OSThreads and Tasks.
   The Task for an OSThread is kept in thread-local storage, and can
   be retrieved at any time using myTask().

   In the THREADED_RTS build, multiple Tasks may all be running
   Haskell code simultaneously. A task relinquishes its Capability
   when it is asked to evaluate an external (C) call.

   Note [Ownership of Task]
   ~~~~~~~~~~~~~~~~~~~~~~~~
   Task ownership is a little tricky.  The default situation is that
   the Task is an OS-thread-local structure that is owned by the OS
   thread named in task->id.  An OS thread not currently executing
   Haskell code might call newBoundTask() at any time, which assumes
   that it has access to the Task for the current OS thread.

   The all_next and all_prev fields of a Task are owned by
   all_tasks_mutex, which must also be taken if we want to create or
   free a Task.

   For an OS thread in Haskell, if (task->cap->running_task != task),
   then the Task is owned by the owner of the parent data structure on
   which it is sleeping; for example, if the task is sleeping on
   spare_workers field of a Capability, then the owner of the
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

     (b) making a foreign call.  The InCall will be on the
         suspended_ccalls list.

   We re-establish ownership in each case by respectively

      (a) the task is currently blocked in yieldCapability().
          This call will return when we have ownership of the Task and
          a Capability.  The Capability we get might not be the same
          as the one we had when we called yieldCapability().

      (b) we must call resumeThread(task), which will safely establish
          ownership of the Task and a Capability.
*/

// The InCall structure represents either a single in-call from C to
// Haskell, or a worker thread.
typedef struct InCall_ {
    StgTSO *   tso;             // the bound TSO (or NULL for a worker)

    StgTSO *   suspended_tso;   // the TSO is stashed here when we
                                // make a foreign call (NULL otherwise);

    Capability *suspended_cap;  // The capability that the
                                // suspended_tso is on, because
                                // we can't read this from the TSO
                                // without owning a Capability in the
                                // first place.

    SchedulerStatus  rstat;     // return status
    StgClosure **    ret;       // return value

    struct Task_ *task;

    // When a Haskell thread makes a foreign call that re-enters
    // Haskell, we end up with another Task associated with the
    // current thread.  We have to remember the whole stack of InCalls
    // associated with the current Task so that we can correctly
    // save & restore the InCall on entry to and exit from Haskell.
    struct InCall_ *prev_stack;

    // Links InCalls onto suspended_ccalls, spare_incalls
    struct InCall_ *prev;
    struct InCall_ *next;
} InCall;

typedef struct Task_ {
#if defined(THREADED_RTS)
    OSThreadId id;              // The OS Thread ID of this task

    // The NUMA node this Task belongs to.  If this is a worker thread, then the
    // OS thread will be bound to this node (see workerStart()).  If this is an
    // external thread calling into Haskell, it can be bound to a node using
    // rts_setInCallCapability().
    uint32_t node;

    Condition cond;             // used for sleeping & waking up this task
    Mutex lock;                 // lock for the condition variable

    // this flag tells the task whether it should wait on task->cond
    // or just continue immediately.  It's a workaround for the fact
    // that signalling a condition variable doesn't do anything if the
    // thread is already running, but we want it to be sticky.
    bool wakeup;
#endif

    // If the task owns a Capability, task->cap points to it.  (occasionally a
    // task may own multiple capabilities, in which case task->cap may point to
    // any of them.  We must be careful to set task->cap to the appropriate one
    // when using Capability APIs.)
    //
    // If the task is a worker, task->cap points to the Capability on which it
    // is queued.
    //
    // If the task is in an unsafe foreign call, then task->cap can be used to
    // retrieve the capability (see rts_unsafeGetMyCapability()).
    struct Capability_ *cap;

    // The current top-of-stack InCall
    struct InCall_ *incall;

    uint32_t n_spare_incalls;
    struct InCall_ *spare_incalls;

    bool    worker;          // == true if this is a worker Task
    bool    stopped;         // == false between newBoundTask and
                                // exitMyTask, or in a worker Task.

    // So that we can detect when a finalizer illegally calls back into Haskell
    bool running_finalizers;

    // if >= 0, this Capability will be used for in-calls
    int preferred_capability;

    // Links tasks on the returning_tasks queue of a Capability, and
    // on spare_workers.
    struct Task_ *next;

    // Links tasks on the all_tasks list; need ACQUIRE_LOCK(&all_tasks_mutex)
    struct Task_ *all_next;
    struct Task_ *all_prev;

} Task;

INLINE_HEADER bool
isBoundTask (Task *task)
{
    return (task->incall->tso != NULL);
}

// A Task is currently a worker if
//  (a) it was created as a worker (task->worker), and
//  (b) it has not left and re-entered Haskell, in which case
//      task->incall->prev_stack would be non-NULL.
//
INLINE_HEADER bool
isWorker (Task *task)
{
    return (task->worker && task->incall->prev_stack == NULL);
}

// Linked list of all tasks.
//
extern Task *all_tasks;

// The all_tasks list is protected by the all_tasks_mutex
#if defined(THREADED_RTS)
extern Mutex all_tasks_mutex;
#endif

// Start and stop the task manager.
// Requires: sched_mutex.
//
void initTaskManager (void);
uint32_t  freeTaskManager (void);

// Create a new Task for a bound thread. This Task must be released
// by calling exitMyTask(). The Task is cached in
// thread-local storage and will remain even after exitMyTask()
// has been called; to free the memory, see freeMyTask().
//
Task* newBoundTask (void);

// Return the current OS thread's Task, which is created if it doesn't already
// exist.  After you have finished using RTS APIs, you should call freeMyTask()
// to release this thread's Task.
Task* getMyTask (void);

// Exit myTask - This is the counterpart of newBoundTask().
void exitMyTask (void);

// Free a Task if one was previously allocated by newBoundTask().
// This is not necessary unless the thread that called newBoundTask()
// will be exiting, or if this thread has finished calling Haskell
// functions.
//
void freeMyTask(void);

// Notify the task manager that a task has stopped.  This is used
// mainly for stats-gathering purposes.
// Requires: sched_mutex.
//
#if defined(THREADED_RTS)
// In the non-threaded RTS, tasks never stop.
void workerTaskStop (Task *task);
#endif

// Put the task back on the free list, mark it stopped.  Used by
// forkProcess().
//
void discardTasksExcept (Task *keep);

// Get the Task associated with the current OS thread (or NULL if none).
//
INLINE_HEADER Task *myTask (void);

#if defined(THREADED_RTS)

// Workers are attached to the supplied Capability.  This Capability
// should not currently have a running_task, because the new task
// will become the running_task for that Capability.
// Requires: sched_mutex.
//
void startWorkerTask (Capability *cap);

// Interrupts a worker task that is performing an FFI call.  The thread
// should not be destroyed.
//
void interruptWorkerTask (Task *task);

#endif /* THREADED_RTS */

// For stats
extern uint32_t taskCount;
extern uint32_t workerCount;
extern uint32_t peakWorkerCount;

// -----------------------------------------------------------------------------
// INLINE functions... private from here on down:

// A thread-local-storage key that we can use to get access to the
// current thread's Task structure.
#if defined(THREADED_RTS)
#if CC_SUPPORTS_TLS
extern __thread Task *my_task;
#else
extern ThreadLocalKey currentTaskKey;
#endif
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
#if defined(THREADED_RTS) && !CC_SUPPORTS_TLS
    return (Task*) getThreadLocalVar(&currentTaskKey);
#else
    return my_task;
#endif
}

INLINE_HEADER void
setMyTask (Task *task)
{
#if defined(THREADED_RTS) && !CC_SUPPORTS_TLS
    setThreadLocalVar(&currentTaskKey,task);
#else
    my_task = task;
#endif
}

// Tasks are identified by their OS thread ID, which can be serialised
// to StgWord64, as defined below.
typedef StgWord64 TaskId;

// Get a unique serialisable representation for a task id.
//
// It's only unique within the process. For example if they are emitted in a
// log file then it is suitable to work out which log entries are releated.
//
// This is needed because OSThreadId is an opaque type
// and in practice on some platforms it is a pointer type.
//
#if defined(THREADED_RTS)
INLINE_HEADER TaskId serialiseTaskId (OSThreadId taskID) {
    // Here OSThreadId may be a pthread_t and pthread_t is a pointer, but within
    // the process we can still use that pointer value as a unique id.
    return (TaskId) (uintptr_t) taskID;
}
#endif

// Get a serialisable Id for the Task's OS thread
// Needed mainly for logging since the OSThreadId is an opaque type
INLINE_HEADER TaskId
serialisableTaskId (Task *task)
{
#if defined(THREADED_RTS)
    return serialiseTaskId(task->id);
#else
    return (TaskId) (uintptr_t) task;
#endif
}

#include "EndPrivate.h"
