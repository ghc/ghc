/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2001
 *
 * Interval timer for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/
#ifndef __ITIMER_H__
#define __ITIMER_H__

extern int startTicker( nat ms, TickProc handle_tick);
extern int stopTicker ( void );

extern lnat getourtimeofday   ( void );
#if 0
/* unused */
extern void block_vtalrm_signal       ( void );
extern void unblock_vtalrm_signal     ( void );
#endif
#endif /* __ITIMER_H__ */
