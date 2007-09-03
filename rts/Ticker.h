/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Interface to the OS-specific implementation of a regular time signal.
 *
 * ---------------------------------------------------------------------------*/

#ifndef TICKER_H
#define TICKER_H

typedef void (*TickProc)(int);

extern void initTicker  (nat ms, TickProc handle_tick);
extern void startTicker (void);
extern void stopTicker  (void);
extern void exitTicker  (void);

#endif /* TICKER_H */
