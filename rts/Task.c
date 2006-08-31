/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2001-2005
 *
 * The task manager subsystem.  Tasks execute STG code, with this
 * module providing the API which the Scheduler uses to control their
 * creation and destruction.
 * 
 * -------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsUtils.h"
#include "OSThreads.h"
#include "Task.h"
#include "Capability.h"
#include "Stats.h"
#include "RtsFlags.h"
#include "Schedule.h"
#include "Hash.h"
#include "Trace.h"

#if HAVE_SIGNAL_H
#include <signal.h>
#endif

// Task lists and global counters.
// Locks required: sched_mutex.
Task *all_tasks = NULL;
static Task *task_free_list = NULL; // singly-linked
static nat taskCount;
static nat tasksRunning;
static nat workerCount;

/* -----------------------------------------------------------------------------
 * Remembering the current thread's Task
 * -------------------------------------------------------------------------- */

// A thread-local-storage key that we can use to get access to the
// current thread's Task structure.
#if defined(THREADED_RTS)
ThreadLocalKey currentTaskKey;
#else
Task *my_task;
#endif

/* -----------------------------------------------------------------------------
 * Rest of the Task API
 * -------------------------------------------------------------------------- */

void
initTaskManager (void)
{
    static int initialized = 0;

    if (!initialized) {
	taskCount = 0;
	workerCount = 0;
	tasksRunning = 0;
	initialized = 1;
#if defined(THREADED_RTS)
	newThreadLocalKey(&currentTaskKey);
#endif
    }
}


void
stopTaskManager (void)
{
    Task *task, *next;

    debugTrace(DEBUG_sched, 
	       "stopping task manager, %d tasks still running",
	       tasksRunning);

    ACQUIRE_LOCK(&sched_mutex);
    for (task = task_free_list; task != NULL; task = next) {
        next = task->next;
#if defined(THREADED_RTS)
        closeCondition(&task->cond);
        closeMutex(&task->lock);
#endif
        stgFree(task);
    }
    task_free_list = NULL;
    RELEASE_LOCK(&sched_mutex);
}


static Task*
newTask (void)
{
#if defined(THREADED_RTS)
    Ticks currentElapsedTime, currentUserTime;
#endif
    Task *task;

    task = stgMallocBytes(sizeof(Task), "newTask");
    
    task->cap  = NULL;
    task->stopped = rtsFalse;
    task->suspended_tso = NULL;
    task->tso  = NULL;
    task->stat = NoStatus;
    task->ret  = NULL;
    
#if defined(THREADED_RTS)
    initCondition(&task->cond);
    initMutex(&task->lock);
    task->wakeup = rtsFalse;
#endif

#if defined(THREADED_RTS)
    currentUserTime = getThreadCPUTime();
    currentElapsedTime = getProcessElapsedTime();
    task->mut_time = 0;
    task->mut_etime = 0;
    task->gc_time = 0;
    task->gc_etime = 0;
    task->muttimestart = currentUserTime;
    task->elapsedtimestart = currentElapsedTime;
#endif

    task->prev = NULL;
    task->next = NULL;
    task->return_link = NULL;

    task->all_link = all_tasks;
    all_tasks = task;

    taskCount++;
    workerCount++;

    return task;
}

Task *
newBoundTask (void)
{
    Task *task;

    ASSERT_LOCK_HELD(&sched_mutex);
    if (task_free_list == NULL) {
	task = newTask();
    } else {
	task = task_free_list;
	task_free_list = task->next;
	task->next = NULL;
	task->prev = NULL;
	task->stopped = rtsFalse;
    }
#if defined(THREADED_RTS)
    task->id = osThreadId();
#endif
    ASSERT(task->cap == NULL);

    tasksRunning++;

    taskEnter(task);

    debugTrace(DEBUG_sched, "new task (taskCount: %d)", taskCount);
    return task;
}

void
boundTaskExiting (Task *task)
{
    task->stopped = rtsTrue;
    task->cap = NULL;

#if defined(THREADED_RTS)
    ASSERT(osThreadId() == task->id);
#endif
    ASSERT(myTask() == task);
    setMyTask(task->prev_stack);

    tasksRunning--;

    // sadly, we need a lock around the free task list. Todo: eliminate.
    ACQUIRE_LOCK(&sched_mutex);
    task->next = task_free_list;
    task_free_list = task;
    RELEASE_LOCK(&sched_mutex);

    debugTrace(DEBUG_sched, "task exiting");
}

#ifdef THREADED_RTS
#define TASK_ID(t) (t)->id
#else
#define TASK_ID(t) (t)
#endif

void
discardTask (Task *task)
{
    ASSERT_LOCK_HELD(&sched_mutex);
    if (!task->stopped) {
	debugTrace(DEBUG_sched, "discarding task %ld", (long)TASK_ID(task));
	task->cap = NULL;
	task->tso = NULL;
	task->stopped = rtsTrue;
	tasksRunning--;
	task->next = task_free_list;
	task_free_list = task;
    }
}

void
taskTimeStamp (Task *task USED_IF_THREADS)
{
#if defined(THREADED_RTS)
    Ticks currentElapsedTime, currentUserTime, elapsedGCTime;

    currentUserTime = getThreadCPUTime();
    currentElapsedTime = getProcessElapsedTime();

    // XXX this is wrong; we want elapsed GC time since the
    // Task started.
    elapsedGCTime = stat_getElapsedGCTime();
    
    task->mut_time = 
	currentUserTime - task->muttimestart - task->gc_time;
    task->mut_etime = 
	currentElapsedTime - task->elapsedtimestart - elapsedGCTime;

    if (task->mut_time  < 0) { task->mut_time  = 0; }
    if (task->mut_etime < 0) { task->mut_etime = 0; }
#endif
}

void
workerTaskStop (Task *task)
{
#if defined(THREADED_RTS)
    OSThreadId id;
    id = osThreadId();
    ASSERT(task->id == id);
    ASSERT(myTask() == task);
#endif

    taskTimeStamp(task);
    task->stopped = rtsTrue;
    tasksRunning--;
}

void
resetTaskManagerAfterFork (void)
{
    // TODO!
    taskCount = 0;
}

#if defined(THREADED_RTS)

void
startWorkerTask (Capability *cap, 
		 void OSThreadProcAttr (*taskStart)(Task *task))
{
  int r;
  OSThreadId tid;
  Task *task;

  workerCount++;

  // A worker always gets a fresh Task structure.
  task = newTask();

  tasksRunning++;

  // The lock here is to synchronise with taskStart(), to make sure
  // that we have finished setting up the Task structure before the
  // worker thread reads it.
  ACQUIRE_LOCK(&task->lock);

  task->cap = cap;

  // Give the capability directly to the worker; we can't let anyone
  // else get in, because the new worker Task has nowhere to go to
  // sleep so that it could be woken up again.
  ASSERT_LOCK_HELD(&cap->lock);
  cap->running_task = task;

  r = createOSThread(&tid, (OSThreadProc *)taskStart, task);
  if (r != 0) {
    sysErrorBelch("failed to create OS thread");
    stg_exit(EXIT_FAILURE);
  }

  debugTrace(DEBUG_sched, "new worker task (taskCount: %d)", taskCount);

  task->id = tid;

  // ok, finished with the Task struct.
  RELEASE_LOCK(&task->lock);
}

#endif /* THREADED_RTS */

#ifdef DEBUG

static void *taskId(Task *task)
{
#ifdef THREADED_RTS
    return (void *)task->id;
#else
    return (void *)task;
#endif
}

void printAllTasks(void);

void
printAllTasks(void)
{
    Task *task;
    for (task = all_tasks; task != NULL; task = task->all_link) {
	debugBelch("task %p is %s, ", taskId(task), task->stopped ? "stopped" : "alive");
	if (!task->stopped) {
	    if (task->cap) {
		debugBelch("on capability %d, ", task->cap->no);
	    }
	    if (task->tso) {
	      debugBelch("bound to thread %lu", (unsigned long)task->tso->id);
	    } else {
		debugBelch("worker");
	    }
	}
	debugBelch("\n");
    }
}		       

#endif

