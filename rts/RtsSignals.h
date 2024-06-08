/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Signal processing / handling.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if !defined(mingw32_HOST_OS) && defined(HAVE_SIGNAL_H)

#include "posix/Signals.h"

#elif defined(mingw32_HOST_OS)

#include "win32/ConsoleHandler.h"

#else

#define signals_pending() (false)

#endif

#if defined(RTS_USER_SIGNALS)

#include "BeginPrivate.h"

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
 * Function: awaitUserSignals()
 *
 * Wait for the next console event. Currently a NOP (returns immediately.)
 */
void awaitUserSignals(void);

/*
 * Function: startPendingSignalHandlers()
 *
 * Start any pending signal handlers. This is used by the scheduler and some
 * in-RTS I/O managers. It does nothing (returns false) in the threaded RTS.
 *
 * Returns true if any signal handlers were pending and thus started.
 */
INLINE_HEADER bool startPendingSignalHandlers(Capability *cap);

#if !defined(THREADED_RTS)
INLINE_HEADER bool startPendingSignalHandlers(Capability *cap)
{
    if (RtsFlags.MiscFlags.install_signal_handlers && signals_pending()) {
        // safe outside the lock
        startSignalHandlers(cap);
        return true;
    } else {
        return false;
    }
}
#else
INLINE_HEADER bool startPendingSignalHandlers(Capability *cap STG_UNUSED)
{
    return false;
}
#endif

#include "EndPrivate.h"

#endif /* RTS_USER_SIGNALS */
