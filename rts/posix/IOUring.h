/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2026
 *
 * An I/O manager based on io_uring
 *
 * Prototypes for functions in IOUring.c
 *
 * -------------------------------------------------------------------------*/

#pragma once

#include "IOManager.h"

#include "BeginPrivate.h"

#if defined(IOMGR_ENABLED_IO_URING)

void initCapabilityIOManagerIOUring(CapIOManager *iomgr);
void freeCapabilityIOManagerIOUring(CapIOManager *iomgr);

/* Synchronous I/O and timer operations */
bool syncIOWaitReadyIOUring(CapIOManager *iomgr, StgTSO *tso,
                         IOReadOrWrite rw, HsInt fd);
void syncIOCancelIOUring(CapIOManager *iomgr, StgTSO *tso);

bool syncDelayTimeoutIOUring(CapIOManager *iomgr, StgTSO *tso, HsInt us_delay);
void syncDelayCancelTimeoutIOUring(CapIOManager *iomgr, StgTSO *tso);

/* Scheduler operations */
void pollCompletedTimeoutsOrIOIOUring(CapIOManager *iomgr);
bool awaitCompletedTimeoutsOrIOIOUring(CapIOManager *iomgr);
void interruptIOManagerIOUring(CapIOManager *iomgr);

#endif /* IOMGR_ENABLED_IO_MANAGER */

#include "EndPrivate.h"