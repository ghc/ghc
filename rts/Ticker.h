/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Interface to the OS-specific implementation of a regular time signal.
 *
 * ---------------------------------------------------------------------------*/

#ifndef TICKER_H
#define TICKER_H

#include "BeginPrivate.h"

typedef void (*TickProc)(int);

void initTicker  (Time interval, TickProc handle_tick);
void startTicker (void);
void stopTicker  (void);
void exitTicker  (bool wait);

#include "EndPrivate.h"

#endif /* TICKER_H */
