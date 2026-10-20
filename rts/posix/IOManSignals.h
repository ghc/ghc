/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005, 2026
 *
 * Shared utilities for I/O managers: signal handling functionality.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

#if !defined(THREADED_RTS)
/*
 * Function: startPendingSignalHandlers()
 *
 * If there are any queued up posix signals, run the handlers
 * associated with them.
 */
void startPendingSignalHandlers(Capability *cap);
#endif

#include "EndPrivate.h"

