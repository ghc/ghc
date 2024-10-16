/* AsyncIO.c
 *
 * Integrating Win32 asynchronous I/O with the GHC RTS.
 *
 * (c) sof, 2002-2003.
 *
 * NOTE: This is the MIO manager, only used for --io-manager=posix.
 *       For the WINIO manager see base in the GHC.Event modules.
 */

#if !defined(THREADED_RTS)

#include "Rts.h"
#include "RtsUtils.h"
#include <windows.h>
#include <stdio.h>
#include "Schedule.h"
#include "Capability.h"
#include "IOManagerInternals.h"
#include "win32/AsyncMIO.h"
#include "win32/MIOManager.h"

/*
 * Overview:
 *
 * Haskell code issue asynchronous I/O requests via the
 * async{Read,Write,DoOp}# primops. These cause addIORequest()
 * to be invoked, which forwards the request to the underlying
 * asynchronous I/O subsystem. Each request is tagged with a unique
 * ID.
 *
 * addIORequest() returns this ID, so that when the blocked CH
 * thread is added onto blocked_queue, its TSO is annotated with
 * it. Upon completion of an I/O request, the async I/O handling
 * code makes a back-call to signal its completion; the local
 * onIOComplete() routine. It adds the IO request ID (along with
 * its result data) to a queue of completed requests before returning.
 *
 * The queue of completed IO request is read by the thread operating
 * the RTS scheduler. It de-queues the CH threads corresponding
 * to the request IDs, making them runnable again.
 *
 */

typedef struct CompletedReq {
    unsigned int   reqID;
    HsInt          len;
    HsInt          errCode;
} CompletedReq;

#define MAX_REQUESTS 200

static Mutex            queue_lock;
static HANDLE           completed_req_event = INVALID_HANDLE_VALUE;
static HANDLE           abandon_req_wait = INVALID_HANDLE_VALUE;
static HANDLE           wait_handles[2];
static CompletedReq     completedTable[MAX_REQUESTS];
static int              completed_hw;
static HANDLE           completed_table_sema;
static int              issued_reqs;

static void
onIOComplete(unsigned int reqID,
             int   fd STG_UNUSED,
             HsInt len,
             void* buf STG_UNUSED,
             HsInt errCode)
{
    DWORD dwRes;
    /* Deposit result of request in queue/table..when there's room. */
    dwRes = WaitForSingleObject(completed_table_sema, INFINITE);
    switch (dwRes) {
    case WAIT_OBJECT_0:
        break;
    default:
        /* Not likely */
        fprintf(stderr,
                "onIOComplete: failed to grab table semaphore (res=%d, err=%ld), "
                "dropping request 0x%lx\n", reqID, dwRes, GetLastError());
        fflush(stderr);
        return;
    }
    OS_ACQUIRE_LOCK(&queue_lock);
    if (completed_hw == MAX_REQUESTS) {
        /* Shouldn't happen */
        fprintf(stderr, "onIOComplete: ERROR -- Request table overflow (%d); "
                        "dropping.\n", reqID);
        fflush(stderr);
    } else {
#if 0
        fprintf(stderr, "onCompl: %d %d %d %d %d\n",
                reqID, len, errCode, issued_reqs, completed_hw);
        fflush(stderr);
#endif
        completedTable[completed_hw].reqID   = reqID;
        completedTable[completed_hw].len     = len;
        completedTable[completed_hw].errCode = errCode;
        completed_hw++;
        issued_reqs--;
        if (completed_hw == 1) {
            /* The event is used to wake up the scheduler thread should it
             * be blocked waiting for requests to complete. The event resets
             * once that thread has cleared out the request queue/table.
             */
            SetEvent(completed_req_event);
        }
    }
    OS_RELEASE_LOCK(&queue_lock);
}

unsigned int
addIORequest(int   fd,
             bool  forWriting,
             bool  isSock,
             HsInt len,
             char* buf)
{
    OS_ACQUIRE_LOCK(&queue_lock);
    issued_reqs++;
    OS_RELEASE_LOCK(&queue_lock);
#if 0
    fprintf(stderr, "addIOReq: %d %d %d\n", fd, forWriting, len);
    fflush(stderr);
#endif
    return AddIORequest(fd,forWriting,isSock,len,buf,onIOComplete);
}

unsigned int
addDelayRequest(HsInt usecs)
{
    OS_ACQUIRE_LOCK(&queue_lock);
    issued_reqs++;
    OS_RELEASE_LOCK(&queue_lock);
#if 0
    fprintf(stderr, "addDelayReq: %d\n", usecs); fflush(stderr);
#endif
    return AddDelayRequest(usecs,onIOComplete);
}

unsigned int
addDoProcRequest(void* proc, void* param)
{
    OS_ACQUIRE_LOCK(&queue_lock);
    issued_reqs++;
    OS_RELEASE_LOCK(&queue_lock);
#if 0
    fprintf(stderr, "addProcReq: %p %p\n", proc, param); fflush(stderr);
#endif
    return AddProcRequest(proc,param,onIOComplete);
}


int
startupAsyncIO(void)
{
    if (!StartIOManager()) {
        return 0;
    }
    OS_INIT_LOCK(&queue_lock);
    /* Create a pair of events:
     *
     *    - completed_req_event -- signals the deposit of request result;
     *                             manual reset.
     *    - abandon_req_wait    -- external OS thread tells current
     *                             RTS/Scheduler thread to abandon wait
     *                             for IO request completion.
     *                             Auto reset.
     */
    completed_req_event = CreateEvent (NULL, TRUE,  FALSE, NULL);
    abandon_req_wait    = CreateEvent (NULL, FALSE, FALSE, NULL);
    wait_handles[0] = completed_req_event;
    wait_handles[1] = abandon_req_wait;
    completed_hw = 0;
    if ( !(completed_table_sema = CreateSemaphore(NULL, MAX_REQUESTS,
                                                  MAX_REQUESTS, NULL)) ) {
        DWORD rc = GetLastError();
        fprintf(stderr, "startupAsyncIO: CreateSemaphore failed 0x%x\n",
                (int)rc);
        fflush(stderr);
    }

    return ( completed_req_event  != INVALID_HANDLE_VALUE &&
             abandon_req_wait     != INVALID_HANDLE_VALUE &&
             completed_table_sema != NULL );
}

void
shutdownAsyncIO(bool wait_threads)
{
    ShutdownIOManager(wait_threads);
    if (completed_req_event != INVALID_HANDLE_VALUE) {
        CloseHandle(completed_req_event);
        completed_req_event = INVALID_HANDLE_VALUE;
    }
    if (abandon_req_wait != INVALID_HANDLE_VALUE) {
        CloseHandle(abandon_req_wait);
        abandon_req_wait = INVALID_HANDLE_VALUE;
    }
    if (completed_table_sema != NULL) {
        CloseHandle(completed_table_sema);
        completed_table_sema = NULL;
    }
    OS_CLOSE_LOCK(&queue_lock);
}

static void
assertValidBlockAsyncFrame(StgPtr sp) {
#if defined(DEBUG)
    StgInfoTable *info = ((StgInfoTable**) sp) [0];
    if (info != &stg_block_async_void_info &&
        info != &stg_block_async_info) {
        barf("assertValidAsyncFrame: invalid frame type");
    }
    if (sp[1] != 0) {
        barf("assertValidAsyncFrame: non-null StgAsyncIOResult");
    }
#else
    (void) sp;
#endif
}

/*
 * Function: awaitRequests(wait)
 *
 * Check for the completion of external IO work requests. Worker
 * threads signal completion of IO requests by depositing them
 * in a table (completedTable). awaitRequests() matches up
 * requests in that table with threads on the blocked_queue,
 * making the threads whose IO requests have completed runnable
 * again.
 *
 * awaitRequests() is called by the scheduler periodically _or_ if
 * it is out of work, and need to wait for the completion of IO
 * requests to make further progress. In the latter scenario,
 * awaitRequests() will simply block waiting for worker threads
 * to complete if the 'completedTable' is empty.
 */
int
awaitRequests(bool wait)
{
#if !defined(THREADED_RTS)
  // none of this is actually used in the threaded RTS

    CapIOManager *iomgr = MainCapability.iomgr;

start:
#if 0
    fprintf(stderr, "awaitRequests(): %d %d %d\n",
            issued_reqs, completed_hw, wait);
    fflush(stderr);
#endif
    OS_ACQUIRE_LOCK(&queue_lock);
    // Nothing immediately available & we won't wait
    if ((!wait && completed_hw == 0)
#if 0
        // If we just return when wait==false, we'll go into a busy
        // wait loop, so I disabled this condition --SDM 18/12/2003
        (issued_reqs == 0 && completed_hw == 0)
#endif
        ) {
        OS_RELEASE_LOCK(&queue_lock);
        return 0;
    }
    if (completed_hw == 0) {
        // empty table, drop lock and wait
        OS_RELEASE_LOCK(&queue_lock);
        if ( wait && getSchedState() == SCHED_RUNNING ) {
            DWORD dwRes = WaitForMultipleObjects(2, wait_handles,
                                                 FALSE, INFINITE);
            switch (dwRes) {
            case WAIT_OBJECT_0:
                // a request was completed
                break;
            case WAIT_OBJECT_0 + 1:
            case WAIT_TIMEOUT:
                // timeout (unlikely) or told to abandon waiting
                return 0;
            case WAIT_FAILED: {
                DWORD dw = GetLastError();
                fprintf(stderr, "awaitRequests: wait failed -- "
                                "error code: %lu\n", dw); fflush(stderr);
                return 0;
            }
            default:
                fprintf(stderr, "awaitRequests: unexpected wait return "
                                "code %lu\n", dwRes); fflush(stderr);
                return 0;
            }
        } else {
            return 0;
        }
        goto start;
    } else {
        int i;
        StgTSO *tso, *prev;

        for (i=0; i < completed_hw; i++) {
            /* For each of the completed requests, match up their Ids
             * with those of the threads on the blocked_queue. If the
             * thread that made the IO request has been subsequently
             * killed (and removed from blocked_queue), no match will
             * be found for that request Id.
             *
             * i.e., killing a Haskell thread doesn't attempt to cancel
             * the IO request it is blocked on.
             *
             */
            unsigned int rID = completedTable[i].reqID;

            prev = NULL;
            for(tso = iomgr->blocked_queue_hd; tso != END_TSO_QUEUE;
                  tso = tso->_link) {

                switch(ACQUIRE_LOAD(&tso->why_blocked)) {
                case BlockedOnRead:
                case BlockedOnWrite:
                case BlockedOnDoProc:
                    if (tso->block_info.async_result->reqID == rID) {
                        // Found the thread blocked waiting on request;
                        // stodgily fill
                        // in its result block.
                        tso->block_info.async_result->len =
                          completedTable[i].len;
                        tso->block_info.async_result->errCode =
                          completedTable[i].errCode;

                        // Drop the matched TSO from blocked_queue
                        if (prev) {
                            setTSOLink(&MainCapability, prev, tso->_link);
                        } else {
                            iomgr->blocked_queue_hd = tso->_link;
                        }
                        if (iomgr->blocked_queue_tl == tso) {
                            iomgr->blocked_queue_tl = prev ? prev
                                                           : END_TSO_QUEUE;
                        }

                        // Terminates the run queue + this inner for-loop.
                        tso->_link = END_TSO_QUEUE;
                        tso->why_blocked = NotBlocked;
                        // save the StgAsyncIOResult in the
                        // stg_block_async_info stack frame, because
                        // the block_info field will be overwritten by
                        // pushOnRunQueue().
                        assertValidBlockAsyncFrame(tso->stackobj->sp);
                        tso->stackobj->sp[1] = (W_)tso->block_info.async_result;
                        pushOnRunQueue(&MainCapability, tso);
                        break;
                    }
                    break;
                default:
                    if (tso->why_blocked != NotBlocked) {
                        barf("awaitRequests: odd thread state");
                    }
                    break;
                }

                prev = tso;
            }
            /* Signal that there's completed table slots available */
            if ( !ReleaseSemaphore(completed_table_sema, 1, NULL) ) {
                DWORD dw = GetLastError();
                fprintf(stderr, "awaitRequests: failed to signal semaphore "
                                "(error code=0x%x)\n", (int)dw);
                fflush(stderr);
            }
        }
        completed_hw = 0;
        ResetEvent(completed_req_event);
        OS_RELEASE_LOCK(&queue_lock);
        return 1;
    }
#endif /* !THREADED_RTS */
}

/*
 * Function: abandonRequestWait()
 *
 * Wake up a thread that's blocked waiting for new IO requests
 * to complete (via awaitRequests().)
 */
void
abandonRequestWait( void )
{
    /* the event is auto-reset, but in case there's no thread
     * already waiting on the event, we want to return it to
     * a non-signalled state.
     *
     * Careful!  There is no synchronisation between
     * abandonRequestWait and awaitRequest, which means that
     * abandonRequestWait might be called just before a thread
     * goes into a wait, and we miss the abandon signal.  So we
     * must SetEvent() here rather than PulseEvent() to ensure
     * that the event isn't lost.  We can re-optimise by resetting
     * the event somewhere safe if we know the event has been
     * properly serviced (see resetAbandon() below).  --SDM 18/12/2003
     */
    SetEvent(abandon_req_wait);
    interruptIOManagerEvent ();
}

void
resetAbandonRequestWait( void )
{
    ResetEvent(abandon_req_wait);
}

#endif /* !defined(THREADED_RTS) */
