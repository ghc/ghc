/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Machine-independent interface to process statistics
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

/* Count of the number of "major" page faults (since process start).
 *
 * In practice, always 0 for Win32 and wasm.
 */
long getPageFaults(void);

#include "EndPrivate.h"
