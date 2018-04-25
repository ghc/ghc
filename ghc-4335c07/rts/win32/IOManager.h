/* IOManager.h
 *
 * Non-blocking / asynchronous I/O for Win32.
 *
 * (c) sof, 2002-2003
 */

#pragma once

#include <windows.h>

/*
 The IOManager subsystem provides a non-blocking view
 of I/O operations. It lets one (or more) OS thread(s)
 issue multiple I/O requests, which the IOManager then
 handles independently of/concurrent to the thread(s)
 that issued the request. Upon completion, the issuing
 thread can inspect the result of the I/O operation &
 take appropriate action.

 The IOManager is intended used with the GHC RTS to
 implement non-blocking I/O in Concurrent Haskell.
 */

/*
 * Our WorkQueue holds WorkItems, encoding IO and
 * delay requests.
 *
 */
typedef void (*CompletionProc)(unsigned int requestID,
                               int   fd,
                               HsInt len,
                               void* buf,
                               HsInt errCode);

/*
 * Asynchronous procedure calls executed by a worker thread
 * take a generic state argument pointer and return an int by
 * default.
 */
typedef int (*DoProcProc)(void *param);

typedef union workData {
    struct {
        int   fd;
        HsInt len;
        char *buf;
    } ioData;
    struct {
        HsInt usecs;
    } delayData;
    struct {
        DoProcProc proc;
        void* param;
    } procData;
} WorkData;

typedef struct WorkItem {
  unsigned int     workKind;
  WorkData         workData;
  unsigned int     requestID;
  CompletionProc   onCompletion;
  unsigned int     abandonOp;
  struct WorkItem  *link;
} WorkItem;

extern CompletionProc onComplete;

/* the kind of operations supported; you could easily imagine
 * that instead of passing a tag describing the work to be performed,
 * a function pointer is passed instead. Maybe later.
 */
#define WORKER_READ        1
#define WORKER_WRITE       2
#define WORKER_DELAY       4
#define WORKER_FOR_SOCKET  8
#define WORKER_DO_PROC    16

/*
 * Starting up and shutting down.
 */
extern bool StartIOManager     ( void );
extern void ShutdownIOManager  ( bool wait_threads );

/*
 * Adding I/O and delay requests. With each request a
 * completion routine is supplied, which the worker thread
 * will invoke upon completion.
 */
extern int AddDelayRequest ( HsInt          usecs,
                             CompletionProc onCompletion);

extern int AddIORequest ( int            fd,
                          bool           forWriting,
                          bool           isSocket,
                          HsInt          len,
                          char*          buffer,
                          CompletionProc onCompletion);

extern int AddProcRequest ( void*          proc,
                            void*          data,
                            CompletionProc onCompletion);

extern void abandonWorkRequest ( int reqID );
