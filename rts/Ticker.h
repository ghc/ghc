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
void exitTicker  (rtsBool wait);

#include "EndPrivate.h"

#endif /* TICKER_H */

// Local Variables:
// mode: C
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
