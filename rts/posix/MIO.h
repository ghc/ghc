/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Signal processing / handling.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "IOManager.h"

#if defined(HAVE_SIGNAL_H)
# include <signal.h>
#endif

#include "BeginPrivate.h"

/* Communicating with the IO manager thread (see GHC.Conc).
 */
void ioManagerWakeup (void);
#if defined(THREADED_RTS)
void ioManagerDie (void);
void ioManagerStart (void);
void ioManagerStartCap (/* inout */ Capability **cap);

void timerManagerNotifySignal(int sig, siginfo_t *info);
#endif

#include "EndPrivate.h"
