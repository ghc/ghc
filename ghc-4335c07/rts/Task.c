/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2001-2005
 *
 * The task manager subsystem.  Tasks execute STG code, with this
 * module providing the API which the Scheduler uses to control their
 * creation and destruction.
 *
 * -------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "Task.h"
#include "Capability.h"
#include "Stats.h"
#include "Schedule.h"
#include "Hash.h"
#include "Trace.h"

#include <string.h>

#if HAVE_SIGNAL_H
#include <signal.h>
#endif

// Task lists and global counters.
// Locks required: all_tasks_mutex.
Task *all_tasks = NULL;

// current number of bound tasks + total number of worker tasks.
uint32_t taskCount;
uint32_t workerCount;
uint32_t currentWorkerCount;
uint32_t peakWorkerCount;

static int tasksInitialized = 0;

static void   freeTask  (Task *task);
static Task * newTask   (bool);

#if defined(THREADED_RTS)
Mutex all_tasks_mutex;
#endif

/* -----------------------------------------------------------------------------
 * Remembering the current thread's Task
 * -------------------------------------------------------------------------- */

// A thread-local-storage key that we can use to get access to the
// current thread's Task structure.
#if defined(THREADED_RTS)
# if defined(MYTASK_USE_TLV)
__thread Task *my_task;
# else
ThreadLocalKey currentTaskKey;
# endif
#else
Task *my_task;
#endif

/* -----------------------------------------------------------------------------
 * Rest of the Task API
 * -------------------------------------------------------------------------- */

void
initTaskManager (void)
{
    if (!tasksInitialized) {
        taskCount = 0;
        workerCount = 0;
        currentWorkerCount = 0;
        peakWorkerCount = 0;
        tasksInitialized = 1;
#if defined(THREADED_RTS)
#if !defined(MYTASK_USE_TLV)
        newThreadLocalKey(&currentTaskKey);
#endif
        initMutex(&all_tasks_mutex);
#endif
    }
}

uint32_t
freeTaskManager (void)
{
    Task *task, *next;
    uint32_t tasksRunning = 0;

    ACQUIRE_LOCK(&all_tasks_mutex);

    for (task = all_tasks; task != NULL; task = next) {
        next = task->all_next;
        if (task->stopped) {
            freeTask(task);
        } else {
            tasksRunning++;
        }
    }

    debugTrace(DEBUG_sched, "freeing task manager, %d tasks still running",
               tasksRunning);

    all_tasks = NULL;

    RELEASE_LOCK(&all_tasks_mutex);

#if defined(THREADED_RTS)
    closeMutex(&all_tasks_mutex);
#if !defined(MYTASK_USE_TLV)
    freeThreadLocalKey(&currentTaskKey);
#endif
#endif

    tasksInitialized = 0;

    return tasksRunning;
}

Task* getTask (void)
{
    Task *task;

    task = myTask();
    if (task != NULL) {
        return task;
    } else {
        task = newTask(false);
#if defined(THREADED_RTS)
        task->id = osThreadId();
#endif
        setMyTask(task);
        return task;
    }
}

void freeMyTask (void)
{
    Task *task;

    task = myTask();

    if (task == NULL) return;

    if (!task->stopped) {
        errorBelch(
            "freeMyTask() called, but the Task is not stopped; ignoring");
        return;
    }

    if (task->worker) {
        errorBelch("freeMyTask() called on a worker; ignoring");
        return;
    }

    ACQUIRE_LOCK(&all_tasks_mutex);

    if (task->all_prev) {
        task->all_prev->all_next = task->all_next;
    } else {
        all_tasks = task->all_next;
    }
    if (task->all_next) {
        task->all_next->all_prev = task->all_prev;
    }

    taskCount--;

    RELEASE_LOCK(&all_tasks_mutex);

    freeTask(task);
    setMyTask(NULL);
}

static void
freeTask (Task *task)
{
    InCall *incall, *next;

    // We only free resources if the Task is not in use.  A
    // Task may still be in use if we have a Haskell thread in
    // a foreign call while we are attempting to shut down the
    // RTS (see conc059).
#if defined(THREADED_RTS)
    closeCondition(&task->cond);
    closeMutex(&task->lock);
#endif

    for (incall = task->incall; incall != NULL; incall = next) {
        next = incall->prev_stack;
        stgFree(incall);
    }
    for (incall = task->spare_incalls; incall != NULL; incall = next) {
        next = incall->next;
        stgFree(incall);
    }

    stgFree(task);
}

static Task*
newTask (bool worker)
{
    Task *task;

#define ROUND_TO_CACHE_LINE(x) ((((x)+63) / 64) * 64)
    task = stgMallocBytes(ROUND_TO_CACHE_LINE(sizeof(Task)), "newTask");

    task->cap           = NULL;
    task->worker        = worker;
    task->stopped       = true;
    task->running_finalizers = false;
    task->n_spare_incalls = 0;
    task->spare_incalls = NULL;
    task->incall        = NULL;
    task->preferred_capability = -1;

#if defined(THREADED_RTS)
    initCondition(&task->cond);
    initMutex(&task->lock);
    task->wakeup = false;
    task->node = 0;
#endif

    task->next = NULL;

    ACQUIRE_LOCK(&all_tasks_mutex);

    task->all_prev = NULL;
    task->all_next = all_tasks;
    if (all_tasks != NULL) {
        all_tasks->all_prev = task;
    }
    all_tasks = task;

    taskCount++;
    if (worker) {
        workerCount++;
        currentWorkerCount++;
        if (currentWorkerCount > peakWorkerCount) {
            peakWorkerCount = currentWorkerCount;
        }
    }
    RELEASE_LOCK(&all_tasks_mutex);

    return task;
}

// avoid the spare_incalls list growing unboundedly
#define MAX_SPARE_INCALLS 8

static void
newInCall (Task *task)
{
    InCall *incall;

    if (task->spare_incalls != NULL) {
        incall = task->spare_incalls;
        task->spare_incalls = incall->next;
        task->n_spare_incalls--;
    } else {
        incall = stgMallocBytes((sizeof(InCall)), "newInCall");
    }

    incall->tso = NULL;
    incall->task = task;
    incall->suspended_tso = NULL;
    incall->suspended_cap = NULL;
    incall->rstat         = NoStatus;
    incall->ret           = NULL;
    incall->next = NULL;
    incall->prev = NULL;
    incall->prev_stack = task->incall;
    task->incall = incall;
}

static void
endInCall (Task *task)
{
    InCall *incall;

    incall = task->incall;
    incall->tso = NULL;
    task->incall = task->incall->prev_stack;

    if (task->n_spare_incalls >= MAX_SPARE_INCALLS) {
        stgFree(incall);
    } else {
        incall->next = task->spare_incalls;
        task->spare_incalls = incall;
        task->n_spare_incalls++;
    }
}


Task *
newBoundTask (void)
{
    Task *task;

    if (!tasksInitialized) {
        errorBelch("newBoundTask: RTS is not initialised; call hs_init() first");
        stg_exit(EXIT_FAILURE);
    }

    task = getTask();

    task->stopped = false;

    newInCall(task);

    debugTrace(DEBUG_sched, "new task (taskCount: %d)", taskCount);
    return task;
}

void
boundTaskExiting (Task *task)
{
#if defined(THREADED_RTS)
    ASSERT(osThreadId() == task->id);
#endif
    ASSERT(myTask() == task);

    endInCall(task);

    // Set task->stopped, but only if this is the last call (#4850).
    // Remember that we might have a worker Task that makes a foreign
    // call and then a callback, so it can transform into a bound
    // Task for the duration of the callback.
    if (task->incall == NULL) {
        task->stopped = true;
    }

    debugTrace(DEBUG_sched, "task exiting");
}


#if defined(THREADED_RTS)
#define TASK_ID(t) (t)->id
#else
#define TASK_ID(t) (t)
#endif

void
discardTasksExcept (Task *keep)
{
    Task *task, *next;

    // Wipe the task list, except the current Task.
    ACQUIRE_LOCK(&all_tasks_mutex);
    for (task = all_tasks; task != NULL; task=next) {
        next = task->all_next;
        if (task != keep) {
            debugTrace(DEBUG_sched, "discarding task %" FMT_SizeT "", (size_t)TASK_ID(task));
#if defined(THREADED_RTS)
            // It is possible that some of these tasks are currently blocked
            // (in the parent process) either on their condition variable
            // `cond` or on their mutex `lock`. If they are we may deadlock
            // when `freeTask` attempts to call `closeCondition` or
            // `closeMutex` (the behaviour of these functions is documented to
            // be undefined in the case that there are threads blocked on
            // them). To avoid this, we re-initialize both the condition
            // variable and the mutex before calling `freeTask` (we do
            // precisely the same for all global locks in `forkProcess`).
            initCondition(&task->cond);
            initMutex(&task->lock);
#endif

            // Note that we do not traceTaskDelete here because
            // we are not really deleting a task.
            // The OS threads for all these tasks do not exist in
            // this process (since we're currently
            // in the child of a forkProcess).
            freeTask(task);
        }
    }
    all_tasks = keep;
    keep->all_next = NULL;
    keep->all_prev = NULL;
    RELEASE_LOCK(&all_tasks_mutex);
}

#if defined(THREADED_RTS)

void
workerTaskStop (Task *task)
{
    DEBUG_ONLY( OSThreadId id );
    DEBUG_ONLY( id = osThreadId() );
    ASSERT(task->id == id);
    ASSERT(myTask() == task);

    ACQUIRE_LOCK(&all_tasks_mutex);

    if (task->all_prev) {
        task->all_prev->all_next = task->all_next;
    } else {
        all_tasks = task->all_next;
    }
    if (task->all_next) {
        task->all_next->all_prev = task->all_prev;
    }

    currentWorkerCount--;

    RELEASE_LOCK(&all_tasks_mutex);

    traceTaskDelete(task);

    freeTask(task);
}

#endif

#if defined(THREADED_RTS)

static void OSThreadProcAttr
workerStart(Task *task)
{
    Capability *cap;

    // See startWorkerTask().
    ACQUIRE_LOCK(&task->lock);
    cap = task->cap;
    RELEASE_LOCK(&task->lock);

    if (RtsFlags.ParFlags.setAffinity) {
        setThreadAffinity(cap->no, n_capabilities);
    }
    if (RtsFlags.GcFlags.numa && !RtsFlags.DebugFlags.numa) {
        setThreadNode(numa_map[task->node]);
    }

    // set the thread-local pointer to the Task:
    setMyTask(task);

    newInCall(task);

    // Everything set up; emit the event before the worker starts working.
    traceTaskCreate(task, cap);

    scheduleWorker(cap,task);
}

void
startWorkerTask (Capability *cap)
{
  int r;
  OSThreadId tid;
  Task *task;

  // A worker always gets a fresh Task structure.
  task = newTask(true);
  task->stopped = false;

  // The lock here is to synchronise with taskStart(), to make sure
  // that we have finished setting up the Task structure before the
  // worker thread reads it.
  ACQUIRE_LOCK(&task->lock);

  // We don't emit a task creation event here, but in workerStart,
  // where the kernel thread id is known.
  task->cap = cap;
  task->node = cap->node;

  // Give the capability directly to the worker; we can't let anyone
  // else get in, because the new worker Task has nowhere to go to
  // sleep so that it could be woken up again.
  ASSERT_LOCK_HELD(&cap->lock);
  cap->running_task = task;

  // Set the name of the worker thread to the original process name followed by
  // ":w", but only if we're on Linux where the program_invocation_short_name
  // global is available.
#if defined(linux_HOST_OS)
  size_t procname_len = strlen(program_invocation_short_name);
  char worker_name[16];
  // The kernel only allocates 16 bytes for thread names, so we truncate if the
  // original name is too long. Process names go in another table that has more
  // capacity.
  if (procname_len >= 13) {
      strncpy(worker_name, program_invocation_short_name, 13);
      strcpy(worker_name + 13, ":w");
  } else {
      strcpy(worker_name, program_invocation_short_name);
      strcpy(worker_name + procname_len, ":w");
  }
#else
  char * worker_name = "ghc_worker";
#endif
  r = createOSThread(&tid, worker_name, (OSThreadProc*)workerStart, task);
  if (r != 0) {
    sysErrorBelch("failed to create OS thread");
    stg_exit(EXIT_FAILURE);
  }

  debugTrace(DEBUG_sched, "new worker task (taskCount: %d)", taskCount);

  task->id = tid;

  // ok, finished with the Task struct.
  RELEASE_LOCK(&task->lock);
}

void
interruptWorkerTask (Task *task)
{
  ASSERT(osThreadId() != task->id);    // seppuku not allowed
  ASSERT(task->incall->suspended_tso); // use this only for FFI calls
  interruptOSThread(task->id);
  debugTrace(DEBUG_sched, "interrupted worker task %#" FMT_HexWord64,
             serialisableTaskId(task));
}

#endif /* THREADED_RTS */

void rts_setInCallCapability (
    int preferred_capability,
    int affinity USED_IF_THREADS)
{
    Task *task = getTask();
    task->preferred_capability = preferred_capability;

#if defined(THREADED_RTS)
    if (affinity) {
        if (RtsFlags.ParFlags.setAffinity) {
            setThreadAffinity(preferred_capability, n_capabilities);
        }
    }
#endif
}

void rts_pinThreadToNumaNode (
    int node USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    if (RtsFlags.GcFlags.numa) {
        Task *task = getTask();
        task->node = capNoToNumaNode(node);
        if (!DEBUG_IS_ON || !RtsFlags.DebugFlags.numa) { // faking NUMA
            setThreadNode(numa_map[task->node]);
        }
    }
#endif
}

#if defined(DEBUG)

void printAllTasks(void);

void
printAllTasks(void)
{
    Task *task;
    for (task = all_tasks; task != NULL; task = task->all_next) {
        debugBelch("task %#" FMT_HexWord64 " is %s, ", serialisableTaskId(task),
                   task->stopped ? "stopped" : "alive");
        if (!task->stopped) {
            if (task->cap) {
                debugBelch("on capability %d, ", task->cap->no);
            }
            if (task->incall->tso) {
              debugBelch("bound to thread %lu",
                         (unsigned long)task->incall->tso->id);
            } else {
                debugBelch("worker");
            }
        }
        debugBelch("\n");
    }
}

#endif
