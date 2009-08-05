/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Signal processing / handling.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTSSIGNALS_H
#define RTSSIGNALS_H

#if !defined(mingw32_HOST_OS)

#include "posix/Signals.h"

#elif defined(mingw32_HOST_OS)

#include "win32/ConsoleHandler.h"

#else

#define signals_pending() (rtsFalse)

#endif

#if RTS_USER_SIGNALS

#pragma GCC visibility push(hidden)

/*
 * Function: initUserSignals()
 *
 * Initialize the console handling substrate.
 */
void initUserSignals(void);

/*
 * Function: initDefaultHandlers()
 *
 * Install any default signal/console handlers. Currently we install a
 * Ctrl+C handler that shuts down the RTS in an orderly manner.
 */
void initDefaultHandlers(void);
void resetDefaultHandlers(void);

void freeSignalHandlers(void);

/*
 * Function: blockUserSignals()
 *
 * Temporarily block the delivery of further console events. Needed to
 * avoid race conditions when GCing the queue of outstanding handlers or
 * when emptying the queue by running the handlers.
 * 
 */
void blockUserSignals(void);

/*
 * Function: unblockUserSignals()
 *
 * The inverse of blockUserSignals(); re-enable the deliver of console events.
 */
void unblockUserSignals(void);

/*
 * Function: awaitUserSignals()
 *
 * Wait for the next console event. Currently a NOP (returns immediately.)
 */
void awaitUserSignals(void);

/*
 * Function: markSignalHandlers()
 *
 * Evacuate the handler queue. _Assumes_ that console event delivery
 * has already been blocked.
 */
void markSignalHandlers (evac_fn evac, void *user);

#pragma GCC visibility pop

#endif /* RTS_USER_SIGNALS */

#endif /* RTSSIGNALS_H */
