/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2006
 *
 * Interval timer service for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/

#ifndef TIMER_H
#define TIMER_H

typedef void (*TickProc)(int);

extern int startTimer(void);
extern int stopTimer(void);

#endif /* TIMER_H */
