/* -----------------------------------------------------------------------------
 * $Id: Itimer.h,v 1.5 2000/03/20 09:42:49 andy Exp $
 *
 * (c) The GHC Team 1998-1999
 *
 * Interval timer for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/

# define TICK_FREQUENCY   50                      /* ticks per second */
# define TICK_MILLISECS   (1000/TICK_FREQUENCY)   /* ms per tick */

extern rtsBool do_prof_ticks;	/* profiling ticks on/off */

nat  initialize_virtual_timer  ( nat ms );
int  install_vtalrm_handler    ( void );
void block_vtalrm_signal       ( void );
void unblock_vtalrm_signal     ( void );
unsigned int getourtimeofday   ( void );
