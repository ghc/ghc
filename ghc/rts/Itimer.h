/* -----------------------------------------------------------------------------
 * $Id: Itimer.h,v 1.2 1998/12/02 13:28:28 simonm Exp $
 *
 * (c) The GHC Team 1998
 *
 * Interval timer for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/

nat  initialize_virtual_timer  ( nat ms );
int  install_vtalrm_handler    ( void (*handler)(int) );
void block_vtalrm_signal       ( void );
void unblock_vtalrm_signal     ( void );


