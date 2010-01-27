/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Interface to the OS-specific implementation of a regular time signal.
 *
 * ---------------------------------------------------------------------------*/

#ifndef TICKER_H
#define TICKER_H

BEGIN_RTS_PRIVATE

typedef void (*TickProc)(int);

void initTicker  (nat ms, TickProc handle_tick);
void startTicker (void);
void stopTicker  (void);
void exitTicker  (rtsBool wait);

END_RTS_PRIVATE

#endif /* TICKER_H */
