/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: inputReady.c,v 1.5 1999/09/12 16:24:46 sof Exp $
 *
 * hReady Runtime Support
 */

/* select and supporting types is not */
#ifndef _AIX
#define NON_POSIX_SOURCE  
#endif

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef _AIX 
/* this is included from sys/types.h only if _BSD is defined. */
/* Since it is not, I include it here. - andre */
#include <sys/select.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

/*
 * inputReady(ptr, msecs) checks to see whether input is available
 * on the file object 'ptr', timing out after (approx.) 'msec' milliseconds.
 * Input meaning 'can I safely read at least a *character* from this file
 * object without blocking?'
 * 
 * If the file object has a non-empty buffer, the test is trivial. If not,
 * we select() on the (readable) file descriptor.
 *
 * Notice that for file descriptors connected to ttys in non-canonical mode
 * (i.e., it's buffered), inputReady will not return true until a *complete
 * line* can be read.
 */

StgInt
inputReady(ptr, msecs)
StgForeignPtr ptr;
StgInt msecs;
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int c, fd, maxfd, ready;
#ifndef mingw32_TARGET_OS
    fd_set rfd;
    struct timeval tv;
#endif

    if ( FILEOBJ_IS_EOF(fo) )
	return 0;

    if ( !FILEOBJ_BUFFER_EMPTY(fo) ) {
	   /* Don't look any further, there's stuff in the buffer */
	   return 1;
    }

#ifdef mingw32_TARGET_OS
    return 1;
#else
    fd = fo->fd;

    /* Now try to get a character */
    FD_ZERO(&rfd);
    FD_SET(fd, &rfd);
    /* select() will consider the descriptor set in the range of 0 to (maxfd-1) */
    maxfd = fd + 1;
    tv.tv_sec  = msecs / 1000;
    tv.tv_usec = msecs % 1000;
    while ((ready = select(maxfd, &rfd, NULL, NULL, &tv)) < 0 ) {
      if (errno != EINTR ) {
		cvtErrno();
		stdErrno();
                ready = -1;
      		break;
      }
   }

    /* 1 => Input ready, 0 => time expired  (-1 error) */
    return (ready);
#endif
}
