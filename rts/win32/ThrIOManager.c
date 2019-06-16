/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2006
 *
 * The IO manager thread in THREADED_RTS.
 * See also libraries/base/GHC/Conc.hs.
 *
 * NOTE: This is used by both MIO and WINIO
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "IOManager.h"
#include "Prelude.h"
#include <windows.h>

// Here's the Event that we use to wake up the IO manager thread
static HANDLE io_manager_event = INVALID_HANDLE_VALUE;

#define EVENT_BUFSIZ 256
Mutex event_buf_mutex;
StgWord32 event_buf[EVENT_BUFSIZ];
uint32_t next_event;

HANDLE
getIOManagerEvent (void)
{
    HANDLE hRes;

    ACQUIRE_LOCK(&event_buf_mutex);

    if (io_manager_event == INVALID_HANDLE_VALUE) {
        hRes = CreateEvent ( NULL, // no security attrs
                             true, // manual reset
                             false, // initial state,
                             NULL ); // event name: NULL for private events
        if (hRes == NULL) {
            sysErrorBelch("getIOManagerEvent");
            stg_exit(EXIT_FAILURE);
        }
        io_manager_event = hRes;
    } else {
        hRes = io_manager_event;
    }

    RELEASE_LOCK(&event_buf_mutex);
    return hRes;
}


HsWord32
readIOManagerEvent (void)
{
    HsWord32 res;

    ACQUIRE_LOCK(&event_buf_mutex);

    if (io_manager_event != INVALID_HANDLE_VALUE) {
        if (next_event == 0) {
            res = 0; // no event to return
        } else {
            do {
               // Dequeue as many wakeup events as possible.
                res = (HsWord32)(event_buf[--next_event]);
            } while (res == IO_MANAGER_WAKEUP && next_event);

            if (next_event == 0) {
                if (!ResetEvent(io_manager_event)) {
                    sysErrorBelch("readIOManagerEvent");
                    stg_exit(EXIT_FAILURE);
                }
            }
        }
    } else {
        res = 0;
    }

    RELEASE_LOCK(&event_buf_mutex);

    //debugBelch("readIOManagerEvent: %d\n", res);
    return res;
}

void
sendIOManagerEvent (HsWord32 event)
{
    ACQUIRE_LOCK(&event_buf_mutex);

    //debugBelch("sendIOManagerEvent: %d to %p\n", event, io_manager_event);
    if (io_manager_event != INVALID_HANDLE_VALUE) {
        if (next_event == EVENT_BUFSIZ) {
            errorBelch("event buffer overflowed; event dropped");
        } else {
            event_buf[next_event++] = (StgWord32)event;
            if (!SetEvent(io_manager_event)) {
                sysErrorBelch("sendIOManagerEvent: SetEvent");
                stg_exit(EXIT_FAILURE);
            }
        }
    }

    RELEASE_LOCK(&event_buf_mutex);
}

void
interruptIOManagerEvent (void)
{
  if (is_io_mng_native_p ()) {
    ACQUIRE_LOCK(&event_buf_mutex);

    /* How expensive is this??.  */
    Capability *cap;
    cap = rts_lock();
    rts_evalIO(&cap, interruptIOManager_closure, NULL);
    rts_unlock(cap);

    RELEASE_LOCK(&event_buf_mutex);
  }
}

void
ioManagerWakeup (void)
{
    sendIOManagerEvent(IO_MANAGER_WAKEUP);
}

void
ioManagerDie (void)
{
    sendIOManagerEvent(IO_MANAGER_DIE);
    // IO_MANAGER_DIE must be idempotent, as it is called
    // repeatedly by shutdownCapability().  Try conc059(threaded1) to
    // illustrate the problem.
    ACQUIRE_LOCK(&event_buf_mutex);
    io_manager_event = INVALID_HANDLE_VALUE;
    RELEASE_LOCK(&event_buf_mutex);
    // ToDo: wait for the IO manager to pick up the event, and
    // then release the Event and Mutex objects we've allocated.
}

void
ioManagerStart (void)
{
#if defined(THREADED_RTS)
    initMutex(&event_buf_mutex);
#endif
    next_event = 0;

    // Make sure the IO manager thread is running
    Capability *cap;
    if (io_manager_event == INVALID_HANDLE_VALUE) {
        cap = rts_lock();
        rts_evalIO(&cap, ensureIOManagerIsRunning_closure, NULL);
        rts_unlock(cap);
    }
}
