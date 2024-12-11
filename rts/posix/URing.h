/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2021-2023
 *
 * An I/O manager based on the Linux io_uring API.
 *
 * Prototypes for functions in URing.c
 *
 * -------------------------------------------------------------------------*/

#pragma once

#include "IOManager.h"

#include "BeginPrivate.h"

#if defined(IOMGR_ENABLED_URING)

void initCapabilityIOManagerURing(Capability *cap, CapIOManager *iomgr);
void initCapabilityIOManagerAfterForkURing(Capability *cap, CapIOManager *iomgr);

/* Synchronous I/O and timer operations */
int syncIOWaitReadyURing(Capability *cap, StgTSO *tso,
                         IOReadOrWrite rw, int fd);

int syncIOReadWriteURing(Capability *cap, StgTSO *tso,
                         IOReadOrWrite rw, int fd,
                         StgClosure *live, void *buf,
                         size_t len, off_t off);

void syncIOCancelURing(Capability *cap, StgTSO *tso);

/* Asynchronous operations */
int asyncIOWaitReadyURing(Capability *cap, StgTSO *tso, StgAsyncIOOp *aiop,
                          IOReadOrWrite rw, int fd);

int asyncIOReadWriteURing(Capability *cap, StgTSO *tso, StgAsyncIOOp *aiop,
                          IOReadOrWrite rw, int fd,
                          StgClosure *live, void *buf,
                          size_t len, off_t off);

void asyncIOCancelURing(Capability *cap, StgAsyncIOOp *aiop);

/* Scheduler operations */
bool anyPendingTimeoutsOrIOURing(CapIOManager *iomgr);
void pollCompletedTimeoutsOrIOURing(Capability *cap);
void awaitCompletedTimeoutsOrIOURing(Capability *cap);

#endif /* IOMGR_ENABLED_URING */

#include "EndPrivate.h"

