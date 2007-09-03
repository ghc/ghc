/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2006
 *
 * Interface to the RTS timer signal (uses OS-dependent Ticker.h underneath)
 *
 * ---------------------------------------------------------------------------*/

#ifndef TIMER_H
#define TIMER_H

extern void initTimer(void);
extern void startTimer(void);
extern void stopTimer(void);
extern void exitTimer(void);

#endif /* TIMER_H */
