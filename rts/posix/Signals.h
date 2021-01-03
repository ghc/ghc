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

/* Communicating with the IO manager thread (see GHC.Conc).
 *
 * TODO: these I/O manager things are not related to signals and ought to live
 * elsewhere, e.g. in a module specifically for the I/O manager.
 */
void ioManagerWakeup (void);
#if defined(THREADED_RTS)
void ioManagerDie (void);
void ioManagerStart (void);
void ioManagerStartCap (/* inout */ Capability **cap);
#endif

extern StgInt *signal_handlers;

#include "EndPrivate.h"
