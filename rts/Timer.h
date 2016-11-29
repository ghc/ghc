/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2006
 *
 * Interface to the RTS timer signal (uses OS-dependent Ticker.h underneath)
 *
 * ---------------------------------------------------------------------------*/

#ifndef TIMER_H
#define TIMER_H

RTS_PRIVATE void initTimer (void);
RTS_PRIVATE void exitTimer (bool wait);

#endif /* TIMER_H */
