/* -----------------------------------------------------------------------------
 * $Id: Signals.h,v 1.4 1999/09/22 11:53:33 sof Exp $
 *
 * (c) The GHC Team, 1998-1999
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

extern void init_shutdown_handler(void);

#else

#define signals_pending() (rtsFalse)

#endif /* PAR */
