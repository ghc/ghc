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

#include "BeginPrivate.h"

bool anyUserHandlers(void);

#if !defined(THREADED_RTS)
extern siginfo_t pending_handler_buf[];
extern siginfo_t *next_pending_handler;
#define signals_pending() (next_pending_handler != pending_handler_buf)
void startSignalHandlers(Capability *cap);
#endif

void install_vtalrm_handler(int sig, TickProc handle_tick);

void ioManagerStartCap (/* inout */ Capability **cap);

extern StgInt *signal_handlers;

#include "EndPrivate.h"
