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
inputReady(int fd, int msecs, int isSock)
{
    if 
#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
    ( isSock ) {
#else
    ( 1 ) {
#endif
	int maxfd, ready;
	fd_set rfd;
	struct timeval tv;
	
	FD_ZERO(&rfd);
	FD_SET(fd, &rfd);
	
	/* select() will consider the descriptor set in the range of 0 to
	 * (maxfd-1) 
	 */
	maxfd = fd + 1;
	tv.tv_sec  = msecs / 1000;
	tv.tv_usec = (msecs % 1000) * 1000;
	
	while ((ready = select(maxfd, &rfd, NULL, NULL, &tv)) < 0 ) {
	    if (errno != EINTR ) {
		return -1;
	    }
	}
	
	/* 1 => Input ready, 0 => not ready, -1 => error */
	return (ready);
    }
#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
    else {
	DWORD rc;
	HANDLE hFile = (HANDLE)_get_osfhandle(fd);
	DWORD avail;

	// WaitForMultipleObjects() works for Console input, but it
	// doesn't work for pipes (it always returns WAIT_OBJECT_0
	// even when no data is available).  There doesn't seem to be
	// an easy way to distinguish the two kinds of HANDLE, so we
	// try to detect pipe input first, and if that fails we try
	// WaitForMultipleObjects().
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
	    if (rc != ERROR_INVALID_HANDLE) {
		return -1;
	    }
	}

	rc = WaitForMultipleObjects( 1,
				     &hFile,
				     TRUE,   /* wait all */
				     msecs); /*millisecs*/
	
	/* 1 => Input ready, 0 => not ready, -1 => error */
	switch (rc) {
	case WAIT_TIMEOUT: return 0;
	case WAIT_OBJECT_0: return 1;
	default: return -1;
	}
    }
#endif
}    
