/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2001-
 *
 * The task manager subsystem.  Tasks execute STG code, with this
 * module providing the API which the Scheduler uses to control their
 * creation and destruction.
 *
 * Two kinds of RTS builds uses 'tasks' - the SMP and the
 * 'native thread-friendly' builds. 
 * 
 * The SMP build lets multiple tasks concurrently execute STG code,
 * all sharing vital internal RTS data structures in a controlled manner
 * (see details elsewhere...ToDo: fill in ref!)
 *
 * The 'threads' build has at any one time only one task executing STG
 * code, other tasks are either busy executing code outside the RTS
 * (e.g., a C call) or waiting for their turn to (again) evaluate some
 * STG code. A task relinquishes its RTS token when it is asked to
 * evaluate an external (C) call.
 *
 * -------------------------------------------------------------------------*/
#include "Rts.h"
#if defined(RTS_SUPPORTS_THREADS) /* to the end */
#include "RtsUtils.h"
#include "OSThreads.h"
#include "Task.h"
#include "Stats.h"
#include "RtsFlags.h"

/* There's not all that much code that is shared between the
 * SMP and threads version of the 'task manager.' A sign
 * that the code ought to be structured differently..(Maybe ToDo).
 */

/* 
 * The following Task Manager-local variables are assumed to be
 * accessed with the RTS lock in hand.
 */
#if defined(SMP)
static TaskInfo* taskTable;
#endif
/* upper bound / the number of tasks created. */
static nat maxTasks;  
/* number of tasks currently created */
static nat taskCount; 
static nat tasksAvailable;


#if defined(SMP)
void
startTaskManager( nat maxCount, void (*taskStart)(void) )
{
  nat i;
  static int initialized = 0;
  
  if (!initialized) {
  
    taskCount = 0;
    maxTasks = maxCount;
    /* allocate table holding task metadata */
  
    if (maxCount > 0) {
      taskTable = stgMallocBytes(maxCount * sizeof(TaskInfo),
			       "startTaskManager:tasks");

      /* and eagerly create them all. */
      for (i = 0; i < maxCount; i++) {
	startTask(taskStart);
	taskCount++;
      }
    }
    initialized = 1;
  }
}

void
startTask ( void (*taskStart)(void) )
{
  int r;
  OSThreadId tid;

  r = createOSThread(&tid,taskStart);
  if (r != 0) {
    barf("startTask: Can't create new task");
  }

  taskTable[taskCount].id = tid;
  taskTable[taskCount].mut_time = 0.0;
  taskTable[taskCount].mut_etime = 0.0;
  taskTable[taskCount].gc_time = 0.0;
  taskTable[taskCount].gc_etime = 0.0;
  taskTable[taskCount].elapsedtimestart = stat_getElapsedTime();

  IF_DEBUG(scheduler,fprintf(stderr,"scheduler: Started task: %ld\n",tid););
  return;
}

void
stopTaskManager ()
{
  nat i;

  /* Don't want to use pthread_cancel, since we'd have to install
   * these silly exception handlers (pthread_cleanup_{push,pop}) around
   * all our locks.
   */
#if 0
  /* Cancel all our tasks */
  for (i = 0; i < RtsFlags.ParFlags.nNodes; i++) {
    pthread_cancel(taskTable[i].id);
  }
  
  /* Wait for all the tasks to terminate */
  for (i = 0; i < maxCount; i++) {
    IF_DEBUG(scheduler,fprintf(stderr,"scheduler: waiting for task %ld\n", 
			       taskTable[i].id));
    pthread_join(taskTable[i].id, NULL);
  }
#endif

  /* Send 'em all a SIGHUP.  That should shut 'em up. */
  await_death = maxCount;
  for (i = 0; i < maxCount; i++) {
    pthread_kill(taskTable[i].id,SIGTERM);
  }
  while (await_death > 0) {
    sched_yield();
  }
  
  return;
}

#else
/************ THREADS version *****************/

void
startTaskManager( nat maxCount, void (*taskStart)(void) )
{
  /* In the threaded case, maxCount is used to limit the
     the creation of worker tasks. Tasks are created lazily, i.e.,
     when the current task gives up the token on executing
     STG code.
  */
  maxTasks = maxCount;
  taskCount = 0;
  tasksAvailable = 0;
}

void
startTask ( void (*taskStart)(void) )
{
  int r;
  OSThreadId tid;
  
  /* Locks assumed: rts_mutex */
  
  /* If there are threads known to be waiting to do
     useful work, no need to create a new task. */
  if (tasksAvailable > 0) {
    IF_DEBUG(scheduler,fprintf(stderr,"scheduler: startTask: %d tasks available, not creating new one.\n",tasksAvailable););
    return;
  }

  /* If the task limit has been reached, just return. */
  if (maxTasks > 0 && taskCount == maxTasks) {
    IF_DEBUG(scheduler,fprintf(stderr,"scheduler: startTask: task limit (%d) reached, not creating new one.\n",maxTasks));
    return;
  }
  

  r = createOSThread(&tid,taskStart);
  if (r != 0) {
    barf("startTask: Can't create new task");
  }
  taskCount++;
  tasksAvailable++;

  IF_DEBUG(scheduler,fprintf(stderr,"scheduler: Started task (%d): %ld\n", taskCount, tid););
  return;
}

/*
 *   When the RTS thread ends up performing a call-out, 
 *   we need to know whether there'll be other tasks/threads
 *   to take over RTS responsibilities. The 'tasksAvailable'
 *   variable holds the number of threads that are _blocked
 *   waiting to enter the RTS_ (or soon will be). Equipped
 *   with that count, startTask() is able to make an informed
 *   decision on whether or not to create a new thread.
 * 
 *   Two functions control increments / decrements of
 *   'tasksAvailable':
 *   
 *      - taskNotAvailable() : called whenever a task/thread
 *        has acquired the RTS lock, i.e., always called by
 *        a thread that holds the rts_mutex lock.
 *
 *      - taskAvailable():     called whenever a task/thread
 *        is about to try to grab the RTS lock. The task manager
 *        and scheduler will only call this whenever it is 
 *        in possession of the rts_mutex lock, i.e.,
 *             - when a new task is created in startTask().
 *             - when the scheduler gives up the RTS token to
 *               let threads waiting to return from an external
 *               call continue.
 * 
 */
void
taskNotAvailable()
{
  if (tasksAvailable > 0) {
    tasksAvailable--;
  }
  return;
}

void
taskAvailable()
{
  tasksAvailable++;
  return;
}



void
stopTaskManager ()
{

}
#endif


nat
getTaskCount ()
{
  return taskCount;
}


#endif /* RTS_SUPPORTS_THREADS */
