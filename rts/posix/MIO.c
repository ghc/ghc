/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * The MIO I/O manager RTS stub.
 *
 * This I/O manager lives primarily in Haskell in the ghc-internal package.
 * This file implementes the required subset of the RTS's internal I/O manager
 * API.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "IOManager.h"

#if defined(IOMGR_ENABLED_MIO_POSIX)

#include "MIO.h"
#include "IOManagerInternals.h"
#include "IOManSignals.h"

#include "Schedule.h"
#include "RtsUtils.h"
#include "Prelude.h"
#include "ThreadLabels.h"

#if defined(HAVE_ERRNO_H)
# include <errno.h>
#endif

#include <stdlib.h>
#include <unistd.h>

#define IO_MANAGER_DIE    0xfe

void initCapabilityIOManagerMIO(CapIOManager *iomgr)
{
    iomgr->control_fd = -1;
    if (iomgr->cap->no == 0) {
        iomgr->shared->timer_manager_control_wr_fd = -1;
    }
}

void setTimerManagerControlFd(int fd) {
    int *timer_manager_control_wr_fd =
            &getCapability(0)->iomgr->shared->timer_manager_control_wr_fd;
    RELAXED_STORE(timer_manager_control_wr_fd, fd);
}

static void timerManagerNotifySignal(int sig, siginfo_t *info,
                                     void *p STG_UNUSED)
{
    StgWord8 buf[sizeof(siginfo_t) + 1];
    int r;

    buf[0] = sig;
    if (info == NULL) {
        // info may be NULL on Solaris (see #3790)
        memset(buf+1, 0, sizeof(siginfo_t));
    } else {
        memcpy(buf+1, info, sizeof(siginfo_t));
    }

    /* In the signal handler we do not have Capability context, so access the
     * control fd via capability 0.
     */
    int *timer_manager_control_wr_fd =
            &getCapability(0)->iomgr->shared->timer_manager_control_wr_fd;
    int timer_control_fd = RELAXED_LOAD(timer_manager_control_wr_fd);
    if (0 <= timer_control_fd)
    {
        r = write(timer_control_fd, buf, sizeof(siginfo_t)+1);
        if (r == -1 && errno == EAGAIN) {
            errorBelch("lost signal due to full pipe: %d\n", sig);
        }
    }

    // If the IO manager hasn't told us what the FD of the write end
    // of its pipe is, there's not much we can do here, so just ignore
    // the signal..
}

int installUserSignalHandlerMIO(int sig, int spi, sigset_t *mask,
                                bool nocldstop)
{
    return installSignalHandler(sig, spi, mask, nocldstop,
                                timerManagerNotifySignal);
}

void
ioManagerDie (void)
{
    finiUserSignals();

    int *timer_manager_control_wr_fd =
            &getCapability(0)->iomgr->shared->timer_manager_control_wr_fd;

    StgWord8 byte = (StgWord8)IO_MANAGER_DIE;
    uint32_t i;
    int r;

    {
        // Shut down timer manager
        const int fd = RELAXED_LOAD(timer_manager_control_wr_fd);
        if (0 <= fd) {
            r = write(fd, &byte, 1);
            if (r == -1) { sysErrorBelch("ioManagerDie: write"); }
            RELAXED_STORE(timer_manager_control_wr_fd, -1);
        }
    }

    {
        // Shut down IO managers
        for (i=0; i < getNumCapabilities(); i++) {
            const int fd = RELAXED_LOAD(&getCapability(i)->iomgr->control_fd);
            if (0 <= fd) {
                r = write(fd, &byte, 1);
                if (r == -1) { sysErrorBelch("ioManagerDie: write"); }
                RELAXED_STORE(&getCapability(i)->iomgr->control_fd, -1);
            }
        }
    }
}

void
ioManagerStartCap (Capability **cap)
{
    rts_evalIO(cap,ensureIOManagerIsRunning_closure,NULL);
}

void
ioManagerStart (void)
{
    initUserSignals();

    int *timer_manager_control_wr_fd =
            &getCapability(0)->iomgr->shared->timer_manager_control_wr_fd;

    // Make sure the IO manager thread is running
    Capability *cap;
    if (SEQ_CST_LOAD(timer_manager_control_wr_fd) < 0) {
        cap = rts_lock();
        ioManagerStartCap(&cap);
        rts_unlock(cap);
    }
}

#else // !IOMGR_ENABLED_MIO_POSIX

/* This is exported from the RTS unconditionally, but only used for MIO. */
void setTimerManagerControlFd(int fd STG_UNUSED) { /* do nothing */ }

#endif // IOMGR_ENABLED_MIO_POSIX
