/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Signal processing / handling.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(HAVE_SIGNAL_H)
# include <signal.h>
#endif

#include "Ticker.h"

bool anyUserHandlers(void);

#if !defined(THREADED_RTS) && defined(RTS_USER_SIGNALS)
extern siginfo_t pending_handler_buf[];
extern siginfo_t *next_pending_handler;
#define signals_pending() (next_pending_handler != pending_handler_buf)
void startSignalHandlers(Capability *cap);
#endif

extern StgInt *signal_handlers;
