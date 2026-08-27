/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2026
 *
 * An I/O manager based on io_uring
 *
 * Prototypes for functions in IOURing.c
 *
 * -------------------------------------------------------------------------*/

#pragma once

#include "IOManager.h"
#include "Rts.h"

#include "BeginPrivate.h"

#if defined(IOMGR_ENABLED_IO_URING)

void initCapabilityIOManagerIOURing(CapIOManager *iomgr);
void freeCapabilityIOManagerIOURing(CapIOManager *iomgr);

/* Synchronous I/O and timer operations */
bool syncIOWaitReadyIOURing(CapIOManager *iomgr, StgTSO *tso, IOReadOrWrite rw,
                            HsInt fd);
void syncIOCancelIOURing(CapIOManager *iomgr, StgTSO *tso);

bool syncDelayTimeoutIOURing(CapIOManager *iomgr, StgTSO *tso, HsInt us_delay);
void syncDelayCancelTimeoutIOURing(CapIOManager *iomgr, StgTSO *tso);

/* Scheduler operations */
void pollCompletedTimeoutsOrIOIOURing(CapIOManager *iomgr);
bool awaitCompletedTimeoutsOrIOIOURing(CapIOManager *iomgr);
void interruptIOManagerIOURing(CapIOManager *iomgr);

#endif /* IOMGR_ENABLED_IO_MANAGER */

#include "EndPrivate.h"