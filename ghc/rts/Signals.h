/* -----------------------------------------------------------------------------
 * $Id: Signals.h,v 1.6 2001/10/31 10:34:29 simonmar Exp $
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

extern void startSignalHandlers(void);
extern void initDefaultHandlers(void);

#else

#define signals_pending() (rtsFalse)

#endif /* PAR */
