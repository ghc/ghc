/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2001-2003
 *
 * Types + prototypes for functions in Task.c
 * (RTS subsystem for handling tasks, agents thay may execute STG code).
 *
 * -------------------------------------------------------------------------*/
#ifndef __TASK_H__
#define __TASK_H__
#if defined(RTS_SUPPORTS_THREADS) /* to the end */

/* 
 * Tasks evaluate STG code; the TaskInfo structure collects together
 * misc metadata about a task.
 * 
 */
typedef struct _TaskInfo {
  OSThreadId id;
  double     elapsedtimestart;
  double     mut_time;
  double     mut_etime;
  double     gc_time;
  double     gc_etime;
} TaskInfo;

extern TaskInfo *taskTable;

extern void startTaskManager ( nat maxTasks, void (*taskStart)(void) );
extern void stopTaskManager ( void );
extern void resetTaskManagerAfterFork ( void );

extern rtsBool startTask ( void (*taskStart)(void) );

#endif /* RTS_SUPPORTS_THREADS */
#endif /* __TASK_H__ */
