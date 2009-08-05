/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Interface to the OS-specific implementation of a regular time signal.
 *
 * ---------------------------------------------------------------------------*/

#ifndef TICKER_H
#define TICKER_H

#pragma GCC visibility push(hidden)

typedef void (*TickProc)(int);

void initTicker  (nat ms, TickProc handle_tick);
void startTicker (void);
void stopTicker  (void);
void exitTicker  (void);

#pragma GCC visibility pop

#endif /* TICKER_H */
