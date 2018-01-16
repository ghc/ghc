/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2006
 *
 * Interface to the RTS timer signal (uses OS-dependent Ticker.h underneath)
 *
 * ---------------------------------------------------------------------------*/

#pragma once

RTS_PRIVATE void initTimer (void);
RTS_PRIVATE void exitTimer (bool wait);
