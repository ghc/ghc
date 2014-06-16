/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2009
 *
 * Interface to the RTS timer signal (uses OS-dependent Ticker.h underneath)
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_TIMER_H
#define RTS_TIMER_H

void startTimer (void);
void stopTimer  (void);
int rtsTimerSignal (void);

#endif /* RTS_TIMER_H */
