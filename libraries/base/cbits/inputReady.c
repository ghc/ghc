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
#ifndef mingw32_TARGET_OS
    ( 1 ) {
#else
    ( isSock ) {
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
#ifdef mingw32_TARGET_OS
    else {
	DWORD rc;
	HANDLE hFile = (HANDLE)_get_osfhandle(fd);
	
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
