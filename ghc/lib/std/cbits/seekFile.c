/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: seekFile.c,v 1.5 2000/01/17 12:30:07 simonmar Exp $
 *
 * hSeek and hIsSeekable Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

/* Invoked by IO.hSeek only */
StgInt
seekFile(StgForeignPtr ptr, StgInt whence, StgInt size, StgByteArray d)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    struct stat sb;
    off_t offset;
    int posn_delta =0;
    int rc = 0;

    switch (whence) {
     case 0:  whence=SEEK_SET; break;
     case 1:  whence=SEEK_CUR; break;
     case 2:  whence=SEEK_END; break;
     default: whence=SEEK_SET; /* Should never happen, really */
    }

    /*
     * We need to snatch the offset out of an MP_INT.  The bits are there sans sign,
     * which we pick up from our size parameter.  If abs(size) is greater than 1,
     * this integer is just too big.
     */

    switch (size) {
    case -1:
	offset = -*(StgInt *) d;
	break;
    case 0:
	offset = 0;
	break;
    case 1:
	offset = *(StgInt *) d;
	break;
    default:
	ghc_errtype = ERR_INVALIDARGUMENT;
	ghc_errstr = "offset out of range";
	return -1;
    }

    /* If we're doing a relative seek, see if we cannot deal 
     * with the request without flushing the buffer..
     *
     * Note: the wording in the report is vague here, but 
     * we only avoid flushing on *input* buffers and *not* output ones.
     */
    if ( whence == SEEK_CUR &&
	 (FILEOBJ_READABLE(fo) && !FILEOBJ_WRITEABLE(fo) &&
	  (fo->bufRPtr + (int)offset) < fo->bufWPtr &&
	  (fo->bufRPtr + (int)offset) >= 0) ) { /* The input buffer case */
       fo->bufRPtr += (int)offset;
       return 0;
    } else if ( whence == SEEK_CUR && (FILEOBJ_READABLE(fo) && !FILEOBJ_WRITEABLE(fo)) ) {
         /* We're seeking outside the input buffer,
	    record delta so that we can adjust the file position
	    reported from the underlying fd to get
	    at the real position we're at when we take into account
	    buffering.
	 */
	posn_delta = fo->bufWPtr - fo->bufRPtr;  /* number of chars left in the buffer */
        if (posn_delta < 0) posn_delta=0;
    }

    /* If we cannot seek within our current buffer, flush it. */
    rc = flushBuffer(ptr);
    if (rc < 0) return rc;

    /* Try to find out the file type */
    while (fstat(fo->fd, &sb) < 0) {
	/* highly unlikely */
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    if (S_ISFIFO(sb.st_mode)) {
	ghc_errtype = ERR_UNSUPPORTEDOPERATION;
	ghc_errstr = "can't seek on a pipe";
	return -1;
    }
    while ( lseek(fo->fd, offset-posn_delta, whence) == -1) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    /* Clear EOF */
    FILEOBJ_CLEAR_EOF(fo);
    return 0;
}

/* Invoked by IO.hSeek only */
StgInt
seekFile_int64(StgForeignPtr ptr, StgInt whence, StgInt64 d)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    struct stat sb;
    off_t offset = d;
    int posn_delta =0;
    int rc = 0;

    switch (whence) {
     case 0: whence=SEEK_SET; break;
     case 1: whence=SEEK_CUR; break;
     case 2: whence=SEEK_END; break;
     default: whence=SEEK_SET; break; /* Should never happen, really */
    }

    /* If we're doing a relative seek, see if we cannot deal 
     * with the request without flushing the buffer..
     *
     * Note: the wording in the report is vague here, but 
     * we only avoid flushing on *input* buffers and *not* output ones.
     */
    if ( whence == SEEK_CUR &&
	 (FILEOBJ_READABLE(fo) && !FILEOBJ_WRITEABLE(fo) &&
	  (fo->bufRPtr + (int)offset) < fo->bufWPtr &&
	  (fo->bufRPtr + (int)offset) >= 0) ) { /* The input buffer case */
       fo->bufRPtr += (int)offset;
       return 0;
    } else if ( whence == SEEK_CUR && (FILEOBJ_READABLE(fo) && !FILEOBJ_WRITEABLE(fo)) ) {
         /* We're seeking outside the input buffer,
	    record delta so that we can adjust the file position
	    reported from the underlying fd to get
	    at the real position we're at when we take into account
	    buffering.
	 */
	posn_delta = fo->bufWPtr - fo->bufRPtr;  /* number of chars left in the buffer */
        if (posn_delta < 0) posn_delta=0;
    }

    /* If we cannot seek within our current buffer, flush it. */
    rc = flushBuffer(ptr);
    if (rc < 0) return rc;

    /* Try to find out the file type & size for a physical file */
    while (fstat(fo->fd, &sb) < 0) {
	/* highly unlikely */
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    if (S_ISFIFO(sb.st_mode)) {
	ghc_errtype = ERR_UNSUPPORTEDOPERATION;
	ghc_errstr = "can't seek on a pipe";
	return -1;
    }
    while ( lseek(fo->fd, offset-posn_delta, whence) == -1) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    /* Clear EOF */
    FILEOBJ_CLEAR_EOF(fo);
    return 0;
}

StgInt
seekFileP(StgForeignPtr ptr)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    struct stat sb;

    /* Try to find out the file type */
    while (fstat(fo->fd, &sb) < 0) {
	/* highly unlikely */
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    /* Pipes are not okay.. */
    if (S_ISFIFO(sb.st_mode)) {
	return 0;
    } 
    /* ..for now, everything else is */
    else {
	return 1;
    }
}
