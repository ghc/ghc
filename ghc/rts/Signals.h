/* -----------------------------------------------------------------------------
 * $Id: Signals.h,v 1.2 1998/12/02 13:28:47 simonm Exp $
 *
 * Signal processing / handling.
 *
 * ---------------------------------------------------------------------------*/

#ifndef PAR

extern StgPtr pending_handler_buf[];
extern StgPtr *next_pending_handler;

#define signals_pending() (next_pending_handler != pending_handler_buf)

extern void initUserSignals(void);
extern void blockUserSignals(void);
extern void unblockUserSignals(void);

/* sig_install declared in PrimOps.h */

extern void start_signal_handlers(void);

#else

#define signals_pending() (rtsFalse)

#endif /* PAR */
