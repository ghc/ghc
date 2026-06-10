/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2006
 *
 * Interface to the RTS timer signal (uses OS-dependent Ticker.h underneath)
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

void initTimer(void);
void exitTimer(bool wait);

void pauseTimer(void);
void unpauseTimer(void);

#include "EndPrivate.h"
