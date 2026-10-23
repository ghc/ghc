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
#if defined(THREADED_RTS)
void initCapabilityIOManagerMIO(CapIOManager *iomgr);

void ioManagerDie (void);
void ioManagerStart (void);
void ioManagerStartCap (/* inout */ Capability **cap);

int installUserSignalHandlerMIO(int sig, int spi, sigset_t *mask,
                                bool nocldstop);
#endif

#include "EndPrivate.h"
