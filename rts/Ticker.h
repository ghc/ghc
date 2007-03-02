/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Ticker interface (implementation is OS-specific)
 *
 * ---------------------------------------------------------------------------*/

#ifndef TICKER_H
#define TICKER_H

extern void startTicker( nat ms, TickProc handle_tick );
extern void stopTicker ( void );

#endif /* TICKER_H */
