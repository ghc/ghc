/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-2002
 *
 * hWaitForInput Runtime Support
 */

/* select and supporting types is not Posix */
/* #include "PosixSource.h" */
#include "HsBase.h"

/*
 * inputReady(fd) checks to see whether input is available on the file
 * descriptor 'fd'.  Input meaning 'can I safely read at least a
 * *character* from this file object without blocking?'
 */
int
fdReady(int fd, int write, int msecs, int isSock)
{
    if 
#if defined(_WIN32)
    ( isSock ) {
#else
    ( 1 ) {
#endif
	int maxfd, ready;
	fd_set rfd, wfd;
	struct timeval tv;
        if ((fd >= (int)FD_SETSIZE) || (fd < 0)) {
            /* avoid memory corruption on too large FDs */
            errno = EINVAL;
            return -1;
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
#if defined(_WIN32)
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
