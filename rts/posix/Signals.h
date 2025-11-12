/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * POSIX signal processing / handling.
 *
 * Most of the API for this is common between POSIX and Win32 console events.
 * The common part of the API lives in RtsSignals.h.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "Ticker.h"

#include "BeginPrivate.h"

void install_vtalrm_handler(int sig, TickProc handle_tick);

#include "EndPrivate.h"
