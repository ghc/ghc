/*
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-2002
 *
 * hWaitForInput Runtime Support
 */

/* FD_SETSIZE defaults to 64 on Windows, which makes even the most basic
 * programs break that use select() on a socket FD.
 * Thus we raise it here (before any #include of network-related headers)
 * to 1024 so that at least those programs would work that would work on
 * Linux if that used select() (luckily it uses poll() by now).
 * See https://gitlab.haskell.org/ghc/ghc/issues/13497#note_140304
 * The real solution would be to remove all uses of select()
 * on Windows, too, and use IO Completion Ports instead.
 * Note that on Windows, one can simply define FD_SETSIZE to the desired
 * size before including Winsock2.h, as described here:
 *   https://msdn.microsoft.com/en-us/library/windows/desktop/ms740141(v=vs.85).aspx
 */
#if defined(_WIN32)
#define FD_SETSIZE 1024
#endif

/* select and supporting types is not Posix */
/* #include <rts/PosixSource.h> */
#include <Rts.h>
#include <limits.h>
#include <stdbool.h>
#include "HsBase.h"
#if !defined(_WIN32)
#include <poll.h>
#endif

/*
 * Returns a timeout suitable to be passed into poll().
 *
 * If `remaining` contains a fractional milliseconds part that cannot be passed
 * to poll(), this function will return the next larger value that can, so
 * that the timeout passed to poll() would always be `>= remaining`.
 *
 * If `infinite`, `remaining` is ignored.
 */
static inline
int
compute_poll_timeout(bool infinite, Time remaining)
{
    if (infinite) return -1;

    if (remaining < 0) return 0;

    if (remaining > MSToTime(INT_MAX)) return INT_MAX;

    int remaining_ms = TimeToMS(remaining);

    if (remaining != MSToTime(remaining_ms)) return remaining_ms + 1;

    return remaining_ms;
}

#if defined(_WIN32)
/*
 * Returns a timeout suitable to be passed into select() on Windows.
 *
 * The given `remaining_tv` serves as a storage for the timeout
 * when needed, but callers should use the returned value instead
 * as it will not be filled in all cases.
 *
 * If `infinite`, `remaining` is ignored and `remaining_tv` not touched
 * (and may be passed as NULL in that case).
 */
static inline
struct timeval *
compute_windows_select_timeout(bool infinite, Time remaining,
                               /* out */ struct timeval * remaining_tv)
{
    if (infinite) {
        return NULL;
    }

    ASSERT(remaining_tv);

    if (remaining < 0) {
        remaining_tv->tv_sec = 0;
        remaining_tv->tv_usec = 0;
    } else if (remaining > MSToTime(LONG_MAX)) {
        remaining_tv->tv_sec = LONG_MAX;
        remaining_tv->tv_usec = LONG_MAX;
    } else {
        remaining_tv->tv_sec  = TimeToMS(remaining) / 1000;
        remaining_tv->tv_usec = TimeToUS(remaining) % 1000000;
    }

    return remaining_tv;
}

/*
 * Returns a timeout suitable to be passed into WaitForSingleObject() on
 * Windows.
 *
 * If `remaining` contains a fractional milliseconds part that cannot be passed
 * to WaitForSingleObject(), this function will return the next larger value
 * that can, so that the timeout passed to WaitForSingleObject() would
 * always be `>= remaining`.
 *
 * If `infinite`, `remaining` is ignored.
 */
static inline
DWORD
compute_WaitForSingleObject_timeout(bool infinite, Time remaining)
{
    // WaitForSingleObject() has the fascinating delicacy behaviour
    // that it waits indefinitely if the `DWORD dwMilliseconds`
    // is set to 0xFFFFFFFF (the maximum DWORD value), which is
    // 4294967295 seconds == ~49.71 days
    // (the Windows API calls this constant INFINITE...).
    //   https://msdn.microsoft.com/en-us/library/windows/desktop/ms687032(v=vs.85).aspx
    //
    // We ensure that if accidentally `remaining == 4294967295`, it does
    // NOT wait forever, by never passing that value to
    // WaitForSingleObject() (so, never returning it from this function),
    // unless `infinite`.

    if (infinite) return INFINITE;

    if (remaining < 0) return 0;

    if (remaining >= MSToTime(INFINITE)) return INFINITE - 1;

    DWORD remaining_ms = TimeToMS(remaining);

    if (remaining != MSToTime(remaining_ms)) return remaining_ms + 1;

    return remaining_ms;
}
#endif

/*
 * inputReady(fd) checks to see whether input is available on the file
 * descriptor 'fd' within 'msecs' milliseconds (or indefinitely if 'msecs' is
 * negative). "Input is available" is defined as 'can I safely read at least a
 * *character* from this file object without blocking?' (this does not work
 * reliably on Linux when the fd is a not-O_NONBLOCK socket, so if you pass
 * socket fds to this function, ensure they have O_NONBLOCK;
 * see `man 2 poll` and `man 2 select`, and
 * https://gitlab.haskell.org/ghc/ghc/issues/13497#note_140309).
 *
 * This function blocks until either `msecs` have passed, or input is
 * available.
 *
 * Returns:
 *   1 => Input ready, 0 => not ready, -1 => error
 * On error, sets `errno`.
 */
int
fdReady(int fd, bool write, int64_t msecs, bool isSock)
{
    bool infinite = msecs < 0;

    // if we need to track the time then record the end time in case we are
    // interrupted.
    Time endTime = 0;
    if (msecs > 0) {
        endTime = getProcessElapsedTime() + MSToTime(msecs);
    }

    // Invariant of all code below:
    // If `infinite`, then `remaining` and `endTime` are never used.

    Time remaining = MSToTime(msecs);

    // Note [Guaranteed syscall time spent]
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // The implementation ensures that if fdReady() is called with N `msecs`,
    // it will not return before an FD-polling syscall *returns*
    // with `endTime` having passed.
    //
    // Consider the following scenario:
    //
    //     1 int ready = poll(..., msecs);
    //     2 if (EINTR happened) {
    //     3   Time now = getProcessElapsedTime();
    //     4   if (now >= endTime) return 0;
    //     5   remaining = endTime - now;
    //     6 }
    //
    // If `msecs` is 5 seconds, but in line 1 poll() returns with EINTR after
    // only 10 ms due to a signal, and if at line 2 the machine starts
    // swapping for 10 seconds, then line 4 will return that there's no
    // data ready, even though by now there may be data ready now, and we have
    // not actually checked after up to `msecs` = 5 seconds whether there's
    // data ready as promised.
    //
    // Why is this important?
    // Assume you call the pizza man to bring you a pizza.
    // You arrange that you won't pay if he doesn't ring your doorbell
    // in under 10 minutes delivery time.
    // At 9:58 fdReady() gets woken by EINTR and then your computer swaps
    // for 3 seconds.
    // At 9:59 the pizza man rings.
    // At 10:01 fdReady() will incorrectly tell you that the pizza man hasn't
    // rung within 10 minutes, when in fact he has.
    //
    // If the pizza man is some watchdog service or dead man's switch program,
    // this is problematic.
    //
    // To avoid it, we ensure that in the timeline diagram:
    //
    //                      endTime
    //                         |
    //     time ----+----------+-------+---->
    //              |                  |
    //       syscall starts     syscall returns
    //
    // the "syscall returns" event is always >= the "endTime" time.
    //
    // In the code this means that we never check whether to `return 0`
    // after a `Time now = getProcessElapsedTime();`, and instead always
    // let the branch marked [we waited the full msecs] handle that case.

#if !defined(_WIN32)
    struct pollfd fds[1];

    fds[0].fd = fd;
    fds[0].events = write ? POLLOUT : POLLIN;
    fds[0].revents = 0;

    // The code below tries to make as few syscalls as possible;
    // in particular, it eschews getProcessElapsedTime() calls
    // when `infinite` or `msecs == 0`.

    // We need to wait in a loop because poll() accepts `int` but `msecs` is
    // `int64_t`, and because signals can interrupt it.

    while (true) {
        int res = poll(fds, 1, compute_poll_timeout(infinite, remaining));

        if (res < 0 && errno != EINTR)
            return (-1); // real error; errno is preserved

        if (res > 0)
            return 1; // FD has new data

        if (res == 0 && !infinite && remaining <= MSToTime(INT_MAX))
            return 0; // FD has no new data and [we waited the full msecs]

        // Non-exit cases
        CHECK( ( res < 0 && errno == EINTR ) || // EINTR happened
               // need to wait more
               ( res == 0 && (infinite ||
                              remaining > MSToTime(INT_MAX)) ) );

        if (!infinite) {
            Time now = getProcessElapsedTime();
            remaining = endTime - now;
        }
    }

#else

    if (isSock) {
        int maxfd;
        fd_set rfd, wfd;
        struct timeval remaining_tv;

        if ((fd >= (int)FD_SETSIZE) || (fd < 0)) {
            barf("fdReady: fd is too big: %d but FD_SETSIZE is %d", fd, (int)FD_SETSIZE);
        }
        FD_ZERO(&rfd);
        FD_ZERO(&wfd);
        if (write) {
            FD_SET(fd, &wfd);
        } else {
            FD_SET(fd, &rfd);
        }

        /* select() will consider the descriptor set in the range of 0 to
         * (maxfd-1)
         */
        maxfd = fd + 1;

        // We need to wait in a loop because the `timeval` `tv_*` members
        // passed into select() accept are `long` (which is 32 bits on 32-bit
        // and 64-bit Windows), but `msecs` is `int64_t`, and because signals
        // can interrupt it.
        //   https://msdn.microsoft.com/en-us/library/windows/desktop/ms740560(v=vs.85).aspx
        //   https://stackoverflow.com/questions/384502/what-is-the-bit-size-of-long-on-64-bit-windows#384672

        while (true) {
            int res = select(maxfd, &rfd, &wfd, NULL,
                             compute_windows_select_timeout(infinite, remaining,
                                                            &remaining_tv));

            if (res < 0 && errno != EINTR)
                return (-1); // real error; errno is preserved

            if (res > 0)
                return 1; // FD has new data

            if (res == 0 && !infinite && remaining <= MSToTime(INT_MAX))
                return 0; // FD has no new data and [we waited the full msecs]

            // Non-exit cases
            CHECK( ( res < 0 && errno == EINTR ) || // EINTR happened
                   // need to wait more
                   ( res == 0 && (infinite ||
                                  remaining > MSToTime(INT_MAX)) ) );

            if (!infinite) {
                Time now = getProcessElapsedTime();
                remaining = endTime - now;
            }
        }

    } else {
        DWORD rc;
        HANDLE hFile = (HANDLE)_get_osfhandle(fd);
        DWORD avail = 0;

        switch (GetFileType(hFile)) {

            case FILE_TYPE_CHAR:
                {
                    INPUT_RECORD buf[1];
                    DWORD count;

                    // nightmare.  A Console Handle will appear to be ready
                    // (WaitForSingleObject() returned WAIT_OBJECT_0) when
                    // it has events in its input buffer, but these events might
                    // not be keyboard events, so when we read from the Handle the
                    // read() will block.  So here we try to discard non-keyboard
                    // events from a console handle's input buffer and then try
                    // the WaitForSingleObject() again.

                    while (1) // keep trying until we find a real key event
                    {
                        rc = WaitForSingleObject(
                            hFile,
                            compute_WaitForSingleObject_timeout(infinite, remaining));
                        switch (rc) {
                            case WAIT_TIMEOUT:
                                // We need to use < here because if remaining
                                // was INFINITE, we'll have waited for
                                // `INFINITE - 1` as per
                                // compute_WaitForSingleObject_timeout(),
                                // so that's 1 ms too little. Wait again then.
                                if (!infinite && remaining < MSToTime(INFINITE))
                                    return 0; // real complete or [we waited the full msecs]
                                goto waitAgain;
                            case WAIT_OBJECT_0: break;
                            default: /* WAIT_FAILED */ maperrno(); return -1;
                        }

                        while (1) // discard non-key events
                        {
                            BOOL success = PeekConsoleInput(hFile, buf, 1, &count);
                            // printf("peek, rc=%d, count=%d, type=%d\n", rc, count, buf[0].EventType);
                            if (!success) {
                                rc = GetLastError();
                                if (rc == ERROR_INVALID_HANDLE || rc == ERROR_INVALID_FUNCTION) {
                                    return 1;
                                } else {
                                    maperrno();
                                    return -1;
                                }
                            }

                            if (count == 0) break; // no more events => wait again

                            // discard console events that are not "key down", because
                            // these will also be discarded by ReadFile().
                            if (buf[0].EventType == KEY_EVENT &&
                                buf[0].Event.KeyEvent.bKeyDown &&
                                buf[0].Event.KeyEvent.uChar.AsciiChar != '\0')
                            {
                                // it's a proper keypress:
                                return 1;
                            }
                            else
                            {
                                // it's a non-key event, a key up event, or a
                                // non-character key (e.g. shift).  discard it.
                                BOOL success = ReadConsoleInput(hFile, buf, 1, &count);
                                if (!success) {
                                    rc = GetLastError();
                                    if (rc == ERROR_INVALID_HANDLE || rc == ERROR_INVALID_FUNCTION) {
                                        return 1;
                                    } else {
                                        maperrno();
                                        return -1;
                                    }
                                }
                            }
                        }

                        Time now;
                    waitAgain:
                        now = getProcessElapsedTime();
                        remaining = endTime - now;
                    }
                }

            case FILE_TYPE_DISK:
                // assume that disk files are always ready:
                return 1;

            case FILE_TYPE_PIPE: {
                // WaitForMultipleObjects() doesn't work for pipes (it
                // always returns WAIT_OBJECT_0 even when no data is
                // available).  If the HANDLE is a pipe, therefore, we try
                // PeekNamedPipe():
                //
                // PeekNamedPipe() does not block, so if it returns that
                // there is no new data, we have to sleep and try again.

                // Because PeekNamedPipe() doesn't block, we have to track
                // manually whether we've called it one more time after `endTime`
                // to fulfill Note [Guaranteed syscall time spent].
                bool endTimeReached = false;
                while (avail == 0) {
                    BOOL success = PeekNamedPipe( hFile, NULL, 0, NULL, &avail, NULL );
                    if (success) {
                        if (avail != 0) {
                            return 1;
                        } else { // no new data
                            if (infinite) {
                                Sleep(1); // 1 millisecond (smallest possible time on Windows)
                                continue;
                            } else if (msecs == 0) {
                                return 0;
                            } else {
                                if (endTimeReached) return 0; // [we waited the full msecs]
                                Time now = getProcessElapsedTime();
                                if (now >= endTime) endTimeReached = true;
                                Sleep(1); // 1 millisecond (smallest possible time on Windows)
                                continue;
                            }
                        }
                    } else {
                        rc = GetLastError();
                        if (rc == ERROR_BROKEN_PIPE) {
                            return 1; // this is probably what we want
                        }
                        if (rc != ERROR_INVALID_HANDLE && rc != ERROR_INVALID_FUNCTION) {
                            maperrno();
                            return -1;
                        }
                    }
                }
            }
            /* PeekNamedPipe didn't work - fall through to the general case */

            default:
                while (true) {
                    rc = WaitForSingleObject(
                        hFile,
                        compute_WaitForSingleObject_timeout(infinite, remaining));

                    switch (rc) {
                        case WAIT_TIMEOUT:
                            // We need to use < here because if remaining
                            // was INFINITE, we'll have waited for
                            // `INFINITE - 1` as per
                            // compute_WaitForSingleObject_timeout(),
                            // so that's 1 ms too little. Wait again then.
                            if (!infinite && remaining < MSToTime(INFINITE))
                                return 0; // real complete or [we waited the full msecs]
                            break;
                        case WAIT_OBJECT_0: return 1;
                        default: /* WAIT_FAILED */ maperrno(); return -1;
                    }

                    // EINTR or a >(INFINITE - 1) timeout completed
                    if (!infinite) {
                        Time now = getProcessElapsedTime();
                        remaining = endTime - now;
                    }
                }
        }
    }
#endif
}
