/* -----------------------------------------------------------------------------
 * $Id: Itimer.h,v 1.10 2001/11/27 01:51:23 sof Exp $
 *
 * (c) The GHC Team 1998-2001
 *
 * Interval timer for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/

# define TICK_FREQUENCY   50                      /* ticks per second */
# define TICK_MILLISECS   (1000/TICK_FREQUENCY)   /* ms per tick */

/* Context switch timing constants. Context switches happen after a
 * whole number of ticks, the default being every tick.
 */
#define CS_MIN_MILLISECS TICK_MILLISECS       /* milliseconds per slice */
 
int  startVirtTimer( nat ms );
int  stopVirtTimer ( void );
void block_vtalrm_signal       ( void );
void unblock_vtalrm_signal     ( void );
unsigned int getourtimeofday   ( void );
