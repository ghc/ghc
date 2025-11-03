/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2025
 *
 * Utilities for a simple fd-based cross-thread wakeup mechanism.
 *
 * This is used to provide a mechanism to wake a thread when it is blocked
 * waiting on fds and timeouts. The mechanism works by including the read end
 * fd into the set of fds the thread waits on, and when a wake up is needed,
 * the write end fd is used.
 *
 * This is implemented using either eventfd() or pipe().
 *
 * Linux 2.6.22+ and FreeBSD 13+ support eventfd. It is a single fd with a
 * 64bit counter. It uses fewer resources than a pipe (less memory and one
 * rather than two fds), and is a tad faster (on the order of 5-10%). Using
 * write() adds to the counter, while read() reads and resets it. Thus
 * multiple writes are combined automatically into a single corresponding
 * read.
 *
 * Otherwise we use a classic unix pipe.
 *
 * In both implementations, multiple sendFdWakeup notifcations (without
 * interleaved collectFdWakeup) are combined to a single notification. This
 * is automatic given the semantics of eventfd, while for pipe we implement
 * it explicitly by draining the pipe in collectFdWakeup.
 *
 * -------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "FdWakeup.h"

#include <fcntl.h>
#include <unistd.h>

#ifdef HAVE_SYS_EVENTFD_H
#include <sys/eventfd.h>
#endif

#if !defined(HAVE_EVENTFD) \
 || (defined(HAVE_EVENTFD) && !(defined(EFD_CLOEXEC) && defined(EFD_NONBLOCK)))
static void fcntl_CLOEXEC_NONBLOCK(int fd)
{
    int res1 = fcntl(fd, F_SETFD, FD_CLOEXEC);
    int res2 = fcntl(fd, F_SETFL, O_NONBLOCK);
    if (RTS_UNLIKELY(res1 < 0 || res2 < 0)) {
        sysErrorBelch("newFdWakeup fcntl()");
        stg_exit(EXIT_FAILURE);
    }
}
#endif

void newFdWakeup(int *wakeup_fd_r, int *wakeup_fd_w)
{
#if defined(HAVE_EVENTFD)
    int wakeup_fd;
#if defined(EFD_CLOEXEC) && defined(EFD_NONBLOCK)
    wakeup_fd = eventfd(0, EFD_CLOEXEC | EFD_NONBLOCK);
#else
    wakeup_fd = eventfd(0, 0);
    if (wakeup_fd >= 0) fcntl_CLOEXEC_NONBLOCK(wakeup_fd);
#endif
    if (RTS_UNLIKELY(wakeup_fd < 0)) {
        sysErrorBelch("newFdWakeup eventfd()");
        stg_exit(EXIT_FAILURE);
    }
    /* eventfd uses the same fd for each end */
    *wakeup_fd_r = wakeup_fd;
    *wakeup_fd_w = wakeup_fd;
#else
    int pipefd[2];
    int res;
    res = pipe(pipefd);
    if (RTS_UNLIKELY(res < 0)) {
        sysErrorBelch("newFdWakeup pipe");
        stg_exit(EXIT_FAILURE);
    }
    fcntl_CLOEXEC_NONBLOCK(pipefd[0]);
    fcntl_CLOEXEC_NONBLOCK(pipefd[1]);
    *wakeup_fd_r = pipefd[0]; /* read end */
    *wakeup_fd_w = pipefd[1]; /* write end */
#endif
}

void closeFdWakeup(int wakeup_fd_r, int wakeup_fd_w)
{
#if defined(HAVE_EVENTFD)
    ASSERT(wakeup_fd_r == wakeup_fd_w);
    close(wakeup_fd_r);
#else
    ASSERT(wakeup_fd_r != wakeup_fd_w);
    close(wakeup_fd_r);
    close(wakeup_fd_w);
#endif
}

/* This is safe to use from a signal handler. Using write() to a pipe
 * or eventfd is fine. */
void sendFdWakeup(int wakeup_fd_w)
{
    int res;
#if defined(HAVE_EVENTFD)
    uint64_t val = 1;
    res = write(wakeup_fd_w, &val, 8);
#else
    unsigned char buf = 1;
    res = write(wakeup_fd_w, &buf, 1);
#endif
    if (RTS_UNLIKELY(res < 0)) {
        /* Unlikely the pipe buffer will fill, but it would not be an error. */
        if (errno == EAGAIN) return;
        sysErrorBelch("sendFdWakeup write");
        stg_exit(EXIT_FAILURE);
    }
}

void collectFdWakeup(int wakeup_fd_r)
{
    int res;
#if defined(HAVE_EVENTFD)
    uint64_t buf;
    /* eventfd combines events into one counter, so a single read is enough */
    res = read(wakeup_fd_r, &buf, 8);
#else
    /* Drain the pipe buffer. Multiple wakeup notifications could
     * have been sent before we have a chance to collect them.
     */
    uint64_t buf;
    do {
        res = read(wakeup_fd_r, &buf, 8);
    } while (res == 8);
#endif
    if (RTS_UNLIKELY(res < 0)) {
        /* After the first pipe read, it could block */
        if (errno == EAGAIN) return;
        sysErrorBelch("collectFdWakeup read");
        stg_exit(EXIT_FAILURE);
    }
}
