/* -----------------------------------------------------------------------------
 * $Id: Itimer.h,v 1.3 1999/02/05 16:02:44 simonm Exp $
 *
 * (c) The GHC Team 1998-1999
 *
 * Interval timer for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/

nat  initialize_virtual_timer  ( nat ms );
int  install_vtalrm_handler    ( void (*handler)(int) );
void block_vtalrm_signal       ( void );
void unblock_vtalrm_signal     ( void );


