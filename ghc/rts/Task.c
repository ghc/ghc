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
#if defined(RTS_SUPPORTS_THREADS) /* to the end */
#include "RtsUtils.h"
#include "OSThreads.h"
#include "Task.h"
#include "Stats.h"
#include "RtsFlags.h"
#include "Schedule.h"
#include "Hash.h"
#include "Capability.h"

#if HAVE_SIGNAL_H
#include <signal.h>
#endif

#define INIT_TASK_TABLE_SIZE 16

TaskInfo* taskTable;
static nat taskTableSize;

// maps OSThreadID to TaskInfo*
HashTable *taskHash;

nat taskCount;
static nat tasksRunning;
static nat workerCount;

#define DEFAULT_MAX_WORKERS 64
nat maxWorkers; // we won't create more workers than this

void
initTaskManager (void)
{
    static int initialized = 0;

    if (!initialized) {
#if defined(SMP)
	taskTableSize = stg_max(INIT_TASK_TABLE_SIZE, 
				RtsFlags.ParFlags.nNodes * 2);
#else
	taskTableSize = INIT_TASK_TABLE_SIZE;
#endif
	taskTable = stgMallocBytes( taskTableSize * sizeof(TaskInfo),
				    "initTaskManager");
    
	taskCount = 0;
	workerCount = 0;
	tasksRunning = 0;

	taskHash = allocHashTable();
  
	maxWorkers = DEFAULT_MAX_WORKERS;

	initialized = 1;
    }
}

static void
expandTaskTable (void)
{
    nat i;

    taskTableSize *= 2;
    taskTable = stgReallocBytes(taskTable, taskTableSize * sizeof(TaskInfo),
				"expandTaskTable");

    /* Have to update the hash table now... */
    for (i = 0; i < taskCount; i++) {
	removeHashTable( taskHash, taskTable[i].id, NULL );
	insertHashTable( taskHash, taskTable[i].id, &taskTable[i] );
    }
}

void
stopTaskManager (void)
{
    IF_DEBUG(scheduler, sched_belch("stopping task manager, %d tasks still running", tasksRunning));
}


rtsBool
startTasks (nat num, void (*taskStart)(void))
{
    nat i; 
    for (i = 0; i < num; i++) {
	if (!startTask(taskStart)) {
	    return rtsFalse;
	}
    }
    return rtsTrue;
}

static TaskInfo*
newTask (OSThreadId id, rtsBool is_worker)
{
    long currentElapsedTime, currentUserTime, elapsedGCTime;
    TaskInfo *task_info;

    if (taskCount >= taskTableSize) {
	expandTaskTable();
    }
    
    insertHashTable( taskHash, id, &(taskTable[taskCount]) );
    
    stat_getTimes(&currentElapsedTime, &currentUserTime, &elapsedGCTime);
    
    task_info = &taskTable[taskCount];
    
    task_info->id = id;
    task_info->is_worker = is_worker;
    task_info->stopped = rtsFalse;
    task_info->mut_time = 0.0;
    task_info->mut_etime = 0.0;
    task_info->gc_time = 0.0;
    task_info->gc_etime = 0.0;
    task_info->muttimestart = currentUserTime;
    task_info->elapsedtimestart = currentElapsedTime;
    
    taskCount++;
    workerCount++;
    tasksRunning++;

    IF_DEBUG(scheduler,sched_belch("startTask: new task %ld (total_count: %d; waiting: %d)\n", id, taskCount, rts_n_waiting_tasks););
    
    return task_info;
}

rtsBool
startTask (void (*taskStart)(void))
{
  int r;
  OSThreadId tid;

  r = createOSThread(&tid,taskStart);
  if (r != 0) {
    barf("startTask: Can't create new task");
  }
  newTask (tid, rtsTrue);
  return rtsTrue;
}

TaskInfo *
threadIsTask (OSThreadId id)
{
    TaskInfo *task_info;
    
    task_info = lookupHashTable(taskHash, id);
    if (task_info != NULL) {
	if (task_info->stopped) {
	    task_info->stopped = rtsFalse;
	}
	return task_info;
    }

    return newTask(id, rtsFalse);
}

TaskInfo *
taskOfId (OSThreadId id)
{
    return lookupHashTable(taskHash, id);
}

void
taskStop (void)
{
    OSThreadId id;
    long currentElapsedTime, currentUserTime, elapsedGCTime;
    TaskInfo *task_info;

    id = osThreadId();
    task_info = taskOfId(id);
    if (task_info == NULL) {
	debugBelch("taskStop: not a task");
	return;
    }
    ASSERT(task_info->id == id);

    stat_getTimes(&currentElapsedTime, &currentUserTime, &elapsedGCTime);
    
    task_info->mut_time = 
	currentUserTime - task_info->muttimestart - task_info->gc_time;
    task_info->mut_etime = 
	currentElapsedTime - task_info->elapsedtimestart - elapsedGCTime;

    if (task_info->mut_time < 0.0)  { task_info->mut_time = 0.0;  }
    if (task_info->mut_etime < 0.0) { task_info->mut_etime = 0.0; }

    task_info->stopped = rtsTrue;
    tasksRunning--;
}

void
resetTaskManagerAfterFork (void)
{
    rts_n_waiting_tasks = 0;
    taskCount = 0;
}

rtsBool
maybeStartNewWorker (void (*taskStart)(void))
{
    /* 
     * If more than one worker thread is known to be blocked waiting
     * on thread_ready_cond, don't create a new one.
     */
    if ( rts_n_waiting_tasks > 0) {
	IF_DEBUG(scheduler,sched_belch(
		     "startTask: %d tasks waiting, not creating new one", 
		     rts_n_waiting_tasks););
	// the task will run as soon as a capability is available,
	// so there's no need to wake it.
	return rtsFalse;
    }
    
    /* If the task limit has been reached, just return. */
    if (maxWorkers > 0 && workerCount >= maxWorkers) {
	IF_DEBUG(scheduler,sched_belch("startTask: worker limit (%d) reached, not creating new one",maxWorkers));
	return rtsFalse;
    }
    
    return startTask(taskStart);
}

#endif /* RTS_SUPPORTS_THREADS */
