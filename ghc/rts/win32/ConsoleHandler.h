/*
 * Console control handler support.
 *
 */
#ifndef __CONSOLEHANDLER_H__
#define __CONSOLEHANDLER_H__

/*
 * Console control handlers lets an application handle Ctrl+C, Ctrl+Break etc.
 * in Haskell under Win32. Akin to the Unix signal SIGINT.
 *
 * The API offered by ConsoleHandler.h is identical to that of the signal handling
 * code (which isn't supported under win32.) Unsurprisingly, the underlying impl 
 * is derived from the signal handling code also.
 */

/*
 * Function: initUserSignals()
 *
 * Initialize the console handling substrate.
 */
extern void initUserSignals(void);

/*
 * Function: initDefaultHandlers()
 *
 * Install any default signal/console handlers. Currently we install a
 * Ctrl+C handler that shuts down the RTS in an orderly manner.
 */
extern void initDefaultHandlers(void);

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
#define anyUserHandlers() (rtsFalse)

/*
 * Function: blockUserSignals()
 *
 * Temporarily block the delivery of further console events. Needed to
 * avoid race conditions when GCing the queue of outstanding handlers or
 * when emptying the queue by running the handlers.
 * 
 */
extern void blockUserSignals(void);

/*
 * Function: unblockUserSignals()
 *
 * The inverse of blockUserSignals(); re-enable the deliver of console events.
 */
extern void unblockUserSignals(void);

/*
 * Function: awaitUserSignals()
 *
 * Wait for the next console event. Currently a NOP (returns immediately.)
 */
extern void awaitUserSignals(void);

/*
 * Function: startSignalHandlers()
 *
 * Run the handlers associated with the queued up console events. Console
 * event delivery is blocked for the duration of this call.
 */
extern void startSignalHandlers(void);

/*
 * Function: markSignalHandlers()
 *
 * Evacuate the handler queue. _Assumes_ that console event delivery
 * has already been blocked.
 */
extern void markSignalHandlers (evac_fn evac);

/*
 * Function: handleSignalsInThisThread()
 * 
 * Have current (OS) thread assume responsibility of handling console events/signals.
 * Currently not used (by the console event handling code.)
 */
extern void handleSignalsInThisThread(void);

/*
 * Function: rts_waitConsoleHandlerCompletion()
 *
 * Esoteric entry point used by worker thread that got woken
 * up as part Ctrl-C delivery.
 */
extern int rts_waitConsoleHandlerCompletion(void);

#endif /* __CONSOLEHANDLER_H__ */
