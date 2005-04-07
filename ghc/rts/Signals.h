/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-1999
 *
 * Signal processing / handling.
 *
 * ---------------------------------------------------------------------------*/

#if !defined(PAR) && !defined(mingw32_HOST_OS)
#define RTS_USER_SIGNALS 1

extern void    initUserSignals(void);
extern void    blockUserSignals(void);
extern void    unblockUserSignals(void);

extern rtsBool anyUserHandlers(void);

#if !defined(RTS_SUPPORTS_THREADS)

extern StgPtr pending_handler_buf[];
extern StgPtr *next_pending_handler;
#define signals_pending() (next_pending_handler != pending_handler_buf)
extern void awaitUserSignals(void);

#else

extern void startSignalHandler(int sig);

#endif

/* sig_install declared in PrimOps.h */

extern void startSignalHandlers(void);
extern void markSignalHandlers (evac_fn evac);
extern void initDefaultHandlers(void);

#elif defined(mingw32_HOST_OS)
#define RTS_USER_SIGNALS 1
#include "win32/ConsoleHandler.h"

#else /* PAR */
#define signals_pending() (rtsFalse)
#define handleSignalsInThisThread() /* nothing */

#endif /* PAR */
