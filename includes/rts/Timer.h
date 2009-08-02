/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2006
 *
 * Interface to the RTS timer signal (uses OS-dependent Ticker.h underneath)
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_TIMER_H
#define RTS_TIMER_H

void startTimer (void);
void stopTimer  (void);

#endif /* RTS_TIMER_H */
