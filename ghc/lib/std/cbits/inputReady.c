/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: inputReady.c,v 1.1 1998/04/10 10:54:40 simonm Exp $
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

StgInt
inputReady(StgAddr fp, StgInt nsecs)
{
    int flags, c, fd, maxfd, ready;
    fd_set rfd;
    struct timeval tv;

    if (feof((FILE *) fp))
	return 0;

    fd = fileno((FILE *)fp);

    /* Get the original file status flags */
    while ((flags = fcntl(fd, F_GETFL)) < 0) {
	/* highly unlikely */
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }

    /* If it's not already non-blocking, make it so */
    if (!(flags & O_NONBLOCK)) {
	while (fcntl(fd, F_SETFL, flags | O_NONBLOCK) < 0) {
	    /* still highly unlikely */
	    if (errno != EINTR) {
		cvtErrno();
		stdErrno();
		return -1;
	    }
	}
    }
    /* Now try to get a character */
    FD_ZERO(&rfd);
    FD_SET(fd, &rfd);
    /* select() will consider the descriptor set in the range of 0 to (maxfd-1) */
    maxfd = fd + 1;
    tv.tv_usec = 0;
    tv.tv_sec  = nsecs;
    while ((ready = select(maxfd, &rfd, NULL, NULL, &tv)) < 0 ) {
      if (errno != EINTR ) {
		cvtErrno();
		stdErrno();
                ready = -1;
      		break;
      }
   }
   /*
    while ((c = getc((FILE *) fp)) == EOF && errno == EINTR)
	clearerr((FILE *) fp);
   */

    /* If we made it non-blocking for this, switch it back */
    if (!(flags & O_NONBLOCK)) {
	while (fcntl(fd, F_SETFL, flags) < 0) {
	    /* still highly unlikely */
	    if (errno != EINTR) {
		cvtErrno();
		stdErrno();
		return -1;
	    }
	}
    }
    /* 1 => Input ready, 0 => time expired  (-1 error) */
    return (ready);

    /*
    if (c == EOF) {
	if (errno == EAGAIN || feof((FILE *) fp)) {
	    clearerr((FILE *) fp);
	    return 0;
	} else {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    } else if (ungetc(c, (FILE *) fp) == EOF) {
	cvtErrno();
	stdErrno();
	return -1;
    } else
	return 1;
    */
}
