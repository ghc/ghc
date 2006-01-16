/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Signal processing / handling.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_SIGNALS_H
#define RTS_SIGNALS_H

#if !defined(PAR) && !defined(mingw32_HOST_OS)

#include "posix/Signals.h"

#elif defined(mingw32_HOST_OS)

#include "win32/ConsoleHandler.h"

#else /* PAR */

#define signals_pending() (rtsFalse)

#endif /* PAR */


#if RTS_USER_SIGNALS

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
 * Function: markSignalHandlers()
 *
 * Evacuate the handler queue. _Assumes_ that console event delivery
 * has already been blocked.
 */
extern void markSignalHandlers (evac_fn evac);

#endif /* RTS_USER_SIGNALS */

#endif /* RTS_SIGNALS_H */
