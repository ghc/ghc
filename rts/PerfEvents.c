/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Support for profiling with the Linux perf_events interface
 *
 * ---------------------------------------------------------------------------*/

#ifdef HAVE_PERF_EVENT

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define _GNU_SOURCE
#define __USE_GNU
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <signal.h>
#include <asm/unistd.h>

#include "Rts.h"
#include "RtsUtils.h"
#include "PerfEvents.h"

static long
perf_event_open(struct perf_event_attr *hw_event, pid_t pid,
                int cpu, int group_fd, unsigned long flags)
{
    int ret;

    ret = syscall(__NR_perf_event_open, hw_event, pid, cpu,
                   group_fd, flags);
    return ret;
}

/* Check whether system supports perf_events */
static StgBool
system_supports_perf(void) {
    struct stat buf;
    if (stat("/proc/sys/kernel/perf_event_paranoid", &buf) != 0) {
        return 0;
    }
    return 1;
}

static void
overflow_handler(int sig STG_UNUSED) {
    // TODO: Collect samples
}

/* Setup a perf_events counter to fire a signal on overflow */
PerfEvent *
setup_event(enum perf_hw_id counter, StgWord64 sample_period) {
    if (!system_supports_perf()) {
        sysErrorBelch("warning: Tried to setup perf_event %d yet system "
                      "doesn't support perf_event interface", counter);
        return NULL;
    }

    PerfEvent *ev = stgMallocBytes(sizeof(PerfEvent), "setup_event");

    struct perf_event_attr pe;
    memset(&pe, 0, sizeof(struct perf_event_attr));
    pe.type = PERF_TYPE_HARDWARE;
    pe.size = sizeof(struct perf_event_attr);
    pe.sample_period = sample_period;
    pe.config = counter;
    pe.precise_ip = 1;
    pe.disabled = 1;
    pe.exclude_kernel = 1;
    pe.exclude_hv = 1;

    // Open control fd
    ev->fd = perf_event_open(&pe, 0, -1, -1, 0);
    if (ev->fd == -1) {
       sysErrorBelch("setup_event: Error opening perf_event %llx\n",
                     pe.config);
       goto fail_free;
    }

    // Configure fd for signal delivery
    struct f_owner_ex owner = {
        .type = F_OWNER_TID,
        .pid = kernelThreadId()
    };
    if (fcntl(ev->fd, F_SETOWN_EX, &owner)) {
        sysErrorBelch("setup_event: fcntl failed");
        goto fail_close;
    }

    // Setup signal handler
    struct sigaction action,oact;
    action.sa_handler = overflow_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    if (sigaction(SIGUSR2, &action, &oact) != 0) {
        sysErrorBelch("setup_event: failed to install SIGUSR2 handler");
        close(ev->fd);
        goto fail_close;
    }

    // Enable event
    ioctl(ev->fd, PERF_EVENT_IOC_RESET, 0);
    ioctl(ev->fd, PERF_EVENT_IOC_ENABLE, 0);

    return ev;

 fail_close:
    close(ev->fd);
 fail_free:
    stgFree(ev);
    return NULL;
}

void
disable_event(PerfEvent *ev) {
    ioctl(ev->fd, PERF_EVENT_IOC_DISABLE, 0);
    close(ev->fd);
    stgFree(ev);
}

#endif
