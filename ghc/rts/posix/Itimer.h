/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2005
 *
 * Interval timer for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/

#ifndef ITIMER_H
#define ITIMER_H

extern lnat getourtimeofday   ( void );
#if 0
/* unused */
extern void block_vtalrm_signal       ( void );
extern void unblock_vtalrm_signal     ( void );
#endif

#endif /* ITIMER_H */
