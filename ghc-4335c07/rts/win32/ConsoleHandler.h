/*
 * Console control handler support.
 *
 */

#pragma once

/*
 * Console control handlers lets an application handle Ctrl+C, Ctrl+Break etc.
 * in Haskell under Win32. Akin to the Unix signal SIGINT.
 *
 * The API offered by ConsoleHandler.h is identical to that of the
 * signal handling code (which isn't supported under win32.)
 * Unsurprisingly, the underlying impl is derived from the signal
 * handling code also.
 */

#if !defined(THREADED_RTS)
/* 
 * under THREADED_RTS, console events are passed to the IO manager
 * thread, which starts up the handler.  See ThrIOManager.c.
 */

/*
 * Function: signals_pending() 
 * 
 * Used by the RTS to check whether new signals have been 'recently' reported.
 * If so, the RTS arranges for the delivered signals to be handled by 
 * de-queueing them from their table, running the associated Haskell 
 * signal handler.
 */
extern StgInt stg_pending_events;

#define signals_pending() ( stg_pending_events > 0)

/* 
 * Function: anyUserHandlers()
 *
 * Used by the Scheduler to decide whether its worth its while to stick
 * around waiting for an external signal when there are no threads
 * runnable. A console handler is used to handle termination events (Ctrl+C)
 * and isn't considered a 'user handler'.
 */
#define anyUserHandlers() (false)

/*
 * Function: startSignalHandlers()
 *
 * Run the handlers associated with the queued up console events. Console
 * event delivery is blocked for the duration of this call.
 */
extern void startSignalHandlers(Capability *cap);

/*
 * Function: rts_waitConsoleHandlerCompletion()
 *
 * Esoteric entry point used by worker thread that got woken
 * up as part Ctrl-C delivery.
 */
extern int rts_waitConsoleHandlerCompletion(void);

#endif /* THREADED_RTS */
