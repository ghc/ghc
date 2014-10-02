/* WorkQueue.h
 *
 * A fixed-size queue; MT-friendly.
 *
 * (c) sof, 2002-2003
 *
 */

#ifndef WIN32_WORKQUEUE_H
#define WIN32_WORKQUEUE_H
#include <windows.h>

/* This is a fixed-size queue. */
#define WORKQUEUE_SIZE 16

typedef HANDLE           Semaphore;
typedef CRITICAL_SECTION CritSection;

typedef struct WorkQueue {
    /* the master lock, need to be grabbed prior to
       using any of the other elements of the struct. */
  CritSection   queueLock;
  /* consumers/workers block waiting for 'workAvailable' */
  Semaphore     workAvailable;
  Semaphore     roomAvailable;
  int           head;
  int           tail;
  void**        items[WORKQUEUE_SIZE];
} WorkQueue;

extern WorkQueue* NewWorkQueue       ( void );
extern void       FreeWorkQueue      ( WorkQueue* pq );
extern HANDLE     GetWorkQueueHandle ( WorkQueue* pq );
extern BOOL       GetWork            ( WorkQueue* pq, void** ppw );
extern BOOL       FetchWork          ( WorkQueue* pq, void** ppw );
extern int        SubmitWork         ( WorkQueue* pq, void*   pw );

#endif /* WIN32_WORKQUEUE_H */
