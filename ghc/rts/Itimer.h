/* -----------------------------------------------------------------------------
 * $Id: Itimer.h,v 1.7 2000/08/25 13:12:07 simonmar Exp $
 *
 * (c) The GHC Team 1998-1999
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
 
extern rtsBool do_prof_ticks;	/* profiling ticks on/off */

/* Total number of ticks since startup */
extern lnat total_ticks;

nat  initialize_virtual_timer  ( nat ms );
int  install_vtalrm_handler    ( void );
void block_vtalrm_signal       ( void );
void unblock_vtalrm_signal     ( void );
unsigned int getourtimeofday   ( void );
