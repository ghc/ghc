/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Signal processing / handling.
 *
 * ---------------------------------------------------------------------------*/

#ifndef POSIX_SIGNALS_H
#define POSIX_SIGNALS_H

extern rtsBool anyUserHandlers(void);

#if !defined(THREADED_RTS)

extern StgPtr pending_handler_buf[];
extern StgPtr *next_pending_handler;
#define signals_pending() (next_pending_handler != pending_handler_buf)
void startSignalHandlers(Capability *cap);

#endif

extern StgInt *signal_handlers;

#endif /* POSIX_SIGNALS_H */

