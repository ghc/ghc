/* -----------------------------------------------------------------------------
 * $Id: Signals.h,v 1.10 2003/03/29 00:00:43 sof Exp $
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

extern void    initUserSignals(void);
extern void    blockUserSignals(void);
extern void    unblockUserSignals(void);

extern rtsBool anyUserHandlers(void);
extern void    awaitUserSignals(void);

/* sig_install declared in PrimOps.h */

extern void startSignalHandlers(void);
extern void markSignalHandlers (evac_fn evac);
extern void initDefaultHandlers(void);

#if !defined(mingw32_TARGET_OS)
extern void handleSignalsInThisThread(void);
#else
#define handleSignalsInThisThread() /* nothing */
#endif

#else

#define signals_pending() (rtsFalse)

#endif /* PAR */
