/* -----------------------------------------------------------------------------
 * $Id: Itimer.h,v 1.11 2003/02/22 04:51:51 sof Exp $
 *
 * (c) The GHC Team 1998-2001
 *
 * Interval timer for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/
#ifndef __ITIMER_H__
#define __ITIMER_H__

extern int startTicker( nat ms );
extern int stopTicker ( void );

extern void block_vtalrm_signal       ( void );
extern void unblock_vtalrm_signal     ( void );
extern unsigned int getourtimeofday   ( void );
#endif /* __ITIMER_H__ */
