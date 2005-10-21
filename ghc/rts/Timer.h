/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2005
 *
 * Interval timer service for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/

#ifndef TIMER_H
#define TIMER_H

# define TICK_MILLISECS   (1000/TICK_FREQUENCY)   /* ms per tick */

/* Context switch timing constants. Context switches happen after a
 * whole number of ticks, the default being every tick.
 */
#define CS_MIN_MILLISECS TICK_MILLISECS       /* milliseconds per slice */

typedef void (*TickProc)(int);

extern int startTimer(nat ms);
extern int stopTimer(void);

#endif /* TIMER_H */
