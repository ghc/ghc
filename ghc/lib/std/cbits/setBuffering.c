/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: setBuffering.c,v 1.6 1999/11/25 16:54:15 simonmar Exp $
 *
 * hSetBuffering Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#define SB_NB (0)
#define SB_LB (-1)
#define SB_BB (-2)

StgInt
setBuffering(ptr, size)
StgForeignPtr ptr;
StgInt size;
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int flags, rc=0;
    int input, isaterm;
    struct termios tio;
    struct stat sb;
   

    /* First off, flush old buffer.. */
    if ( (fo->flags & FILEOBJ_WRITE) ) {
       rc = flushBuffer(ptr);
    }
    if (rc<0) return rc;

    /* Let go of old buffer, and reset buffer pointers. */
    if ( fo->buf != NULL ) {
       free(fo->buf);
       fo->bufWPtr = 0;
       fo->bufRPtr = 0;
       fo->bufSize = 0;
       fo->buf     = NULL;
    }

#ifndef mingw32_TARGET_OS
    while ((flags = fcntl(fo->fd, F_GETFL)) < 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    flags &= O_ACCMODE;
    input = flags == O_RDONLY || flags == O_RDWR;

    isaterm = input && isatty(fo->fd);
#endif

    switch (size) {
    case SB_NB:
        fo->flags &= ~FILEOBJ_LB & ~FILEOBJ_BB;

#ifndef mingw32_TARGET_OS
	if (isaterm) {
	    /* Switch over to canonical mode. */
	    if (tcgetattr(fo->fd, &tio) < 0) {
		cvtErrno();
		stdErrno();
		return -1;
	    }
	    tio.c_lflag &=  ~ICANON;
	    tio.c_cc[VMIN] = 1;
	    tio.c_cc[VTIME] = 0;
	    if (tcsetattr(fo->fd, TCSANOW, &tio) < 0) {
		cvtErrno();
		stdErrno();
		return -1;
	    }
	}
#endif
	return 0;
    case SB_LB:
        fo->flags &= ~FILEOBJ_BB;
	fo->flags |= FILEOBJ_LB;
        size = BUFSIZ;
	break;
    case SB_BB:

#ifdef HAVE_ST_BLKSIZE
	while (fstat(fo->fd, &sb) < 0) {
	   /* not very likely.. */
	   if ( errno != EINTR ) {
	      cvtErrno();
	      stdErrno();
	      return -1;
	   }
        }
	size = sb.st_blksize;
#else
	size = BUFSIZ;
#endif
        fo->flags &= ~FILEOBJ_LB;
	fo->flags |= FILEOBJ_BB;
	/* fall through */
    default:
	break;
    }
  
    if ( size > 0) {
       fo->buf = malloc(size*sizeof(char));
       if (fo->buf == NULL) {
           return -1;
       }
       fo->bufSize = size;
    }
#ifndef mingw32_TARGET_OS
    if (isaterm) {

	/*
	 * Try to switch back to cooked mode.
	 */

	if (tcgetattr(fo->fd, &tio) < 0) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
	tio.c_lflag |= ICANON;
	if (tcsetattr(fo->fd, TCSANOW, &tio) < 0) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
#endif
    return 0;
}

StgInt const_BUFSIZ() { return BUFSIZ; }

