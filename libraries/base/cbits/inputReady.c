/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-2002
 *
 * hWaitForInput Runtime Support
 */

/* select and supporting types is not Posix */
/* #include "PosixSource.h" */
#include "HsBase.h"
#if !defined(_WIN32)
#include <poll.h>
#include <sys/time.h>
#endif

/*
 * inputReady(fd) checks to see whether input is available on the file
 * descriptor 'fd' within 'msecs' milliseconds (or indefinitely if 'msecs' is
 * negative). "Input is available" is defined as 'can I safely read at least a
 * *character* from this file object without blocking?'
 */
int
fdReady(int fd, int write, int msecs, int isSock)
{

#if !defined(_WIN32)
    struct pollfd fds[1];

    // if we need to track the then record the current time in case we are
    // interrupted.
    struct timeval tv0;
    if (msecs > 0) {
        if (gettimeofday(&tv0, NULL) != 0) {
            fprintf(stderr, "fdReady: gettimeofday failed: %s\n",
                    strerror(errno));
            abort();
        }
    }

    fds[0].fd = fd;
    fds[0].events = write ? POLLOUT : POLLIN;
    fds[0].revents = 0;

    int res;
    while ((res = poll(fds, 1, msecs)) < 0) {
        if (errno == EINTR) {
            if (msecs > 0) {
                struct timeval tv;
                if (gettimeofday(&tv, NULL) != 0) {
                    fprintf(stderr, "fdReady: gettimeofday failed: %s\n",
                            strerror(errno));
                    abort();
                }

                int elapsed = 1000 * (tv.tv_sec - tv0.tv_sec)
                            + (tv.tv_usec - tv0.tv_usec) / 1000;
                msecs -= elapsed;
                if (msecs <= 0) return 0;
                tv0 = tv;
            }
        } else {
            return (-1);
        }
    }

    // res is the number of FDs with events
    return (res > 0);

#else

    if (isSock) {
	int maxfd, ready;
	fd_set rfd, wfd;
	struct timeval tv;
        if ((fd >= (int)FD_SETSIZE) || (fd < 0)) {
            fprintf(stderr, "fdReady: fd is too big");
            abort();
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
	tv.tv_sec  = msecs / 1000;
	tv.tv_usec = (msecs % 1000) * 1000;
	
	while ((ready = select(maxfd, &rfd, &wfd, NULL, &tv)) < 0 ) {
	    if (errno != EINTR ) {
		return -1;
	    }
	}
	
	/* 1 => Input ready, 0 => not ready, -1 => error */
	return (ready);
    }
    else {
	DWORD rc;
	HANDLE hFile = (HANDLE)_get_osfhandle(fd);
	DWORD avail;

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
                rc = WaitForSingleObject( hFile, msecs );
                switch (rc) {
                case WAIT_TIMEOUT: return 0;
                case WAIT_OBJECT_0: break;
                default: /* WAIT_FAILED */ maperrno(); return -1;
                }

                while (1) // discard non-key events
                {
                    rc = PeekConsoleInput(hFile, buf, 1, &count);
                    // printf("peek, rc=%d, count=%d, type=%d\n", rc, count, buf[0].EventType);
                    if (rc == 0) {
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
                        rc = ReadConsoleInput(hFile, buf, 1, &count);
                        if (rc == 0) {
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
            }
        }

        case FILE_TYPE_DISK:
            // assume that disk files are always ready:
            return 1;

        case FILE_TYPE_PIPE:
            // WaitForMultipleObjects() doesn't work for pipes (it
            // always returns WAIT_OBJECT_0 even when no data is
            // available).  If the HANDLE is a pipe, therefore, we try
            // PeekNamedPipe:
            //
            rc = PeekNamedPipe( hFile, NULL, 0, NULL, &avail, NULL );
            if (rc != 0) {
                if (avail != 0) {
                    return 1;
                } else {
                    return 0;
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
            /* PeekNamedPipe didn't work - fall through to the general case */

        default:
            rc = WaitForSingleObject( hFile, msecs );

            /* 1 => Input ready, 0 => not ready, -1 => error */
            switch (rc) {
            case WAIT_TIMEOUT: return 0;
            case WAIT_OBJECT_0: return 1;
            default: /* WAIT_FAILED */ maperrno(); return -1;
            }
        }
    }
#endif
}    
