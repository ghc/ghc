/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2020-2023
 *
 * An I/O manager based on the classic Unix poll() system call.
 *
 * Prototypes for functions in Poll.c
 *
 * -------------------------------------------------------------------------*/

#pragma once

#include "IOManager.h"

#include "BeginPrivate.h"

#if defined(IOMGR_ENABLED_POLL)

void initCapabilityIOManagerPoll(CapIOManager *iomgr);

/* Synchronous I/O and timer operations */
bool syncIOWaitReadyPoll(CapIOManager *iomgr, StgTSO *tso,
                         IOReadOrWrite rw, HsInt fd);
void syncIOCancelPoll(CapIOManager *iomgr, StgTSO *tso);

/* Asynchronous operations */
bool asyncIOWaitReadyPoll(CapIOManager *iomgr, StgAsyncIOOp *aiop,
                          IOReadOrWrite rw, int fd);
void asyncIOCancelPoll(CapIOManager *iomgr, StgAsyncIOOp *aiop);

/* Scheduler operations */
bool anyPendingTimeoutsOrIOPoll(CapIOManager *iomgr);
void pollCompletedTimeoutsOrIOPoll(CapIOManager *iomgr);
void awaitCompletedTimeoutsOrIOPoll(CapIOManager *iomgr);

#endif /* IOMGR_ENABLED_POLL */

#include "EndPrivate.h"

