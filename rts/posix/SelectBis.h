/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2020-2026
 *
 * A second I/O manager based on the classic Unix select() system call.
 *
 * This I/O manager is called "selectbis", because it is the second such I/O
 * manager based on select(). The historic implementation is named "select"
 * and lives in Select.{c,h}. This I/O manager exists for the benefit of users
 * of Apple products.
 *
 * The poll I/O manger _should_ be the portable baseline posix I/O manager.
 * Unfortunately Mac OSX has a buggy implementation of poll(). The OSX man
 * page documents this as:
 *
 * > BUGS The poll() system call currently does not support devices.
 *
 * This is quite incredible, given that poll and select should be relatively
 * thin interfaces to the the same underlying kernel infrastructure.
 * Furthermore, OSX is supposedly certified as POSIX compliant! Due to this
 * (incompetence) we need a new I/O manager implementation based on the
 * antique select() API, with all of its known limitations.
 *
 * Please direct all complaints to:
 *   Apple Inc., One Apple Park Way, Cupertino, CA 95014, USA.
 *
 * Prototypes for functions in SelectBis.c
 *
 * -------------------------------------------------------------------------*/

#pragma once

#include "IOManager.h"

#include "BeginPrivate.h"

#if defined(IOMGR_ENABLED_SELECTBIS)

void initCapabilityIOManagerSelectBis(CapIOManager *iomgr);
void freeCapabilityIOManagerSelectBis(CapIOManager *iomgr);

/* Synchronous I/O and timer operations */
bool syncIOWaitReadySelectBis(CapIOManager *iomgr, StgTSO *tso,
                              IOReadOrWrite rw, HsInt fd);
void syncIOCancelSelectBis(CapIOManager *iomgr, StgTSO *tso);

/* Asynchronous operations */
bool asyncIOWaitReadySelectBis(CapIOManager *iomgr, StgAsyncIOOp *aiop,
                               IOReadOrWrite rw, int fd);
void asyncIOCancelSelectBis(CapIOManager *iomgr, StgAsyncIOOp *aiop);

/* Scheduler operations */
bool anyPendingTimeoutsOrIOSelectBis(CapIOManager *iomgr);
void pollCompletedTimeoutsOrIOSelectBis(CapIOManager *iomgr);
bool awaitCompletedTimeoutsOrIOSelectBis(CapIOManager *iomgr);
void interruptIOManagerSelectBis(CapIOManager *iomgr);

#endif /* IOMGR_ENABLED_SELECTBIS */

#include "EndPrivate.h"

