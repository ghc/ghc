/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: filePosn.c,v 1.6 1999/12/08 15:47:07 simonmar Exp $
 *
 * hGetPosn and hSetPosn Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

StgInt
getFilePosn(ptr)
StgForeignPtr ptr;
{
    IOFileObject* fo = (IOFileObject*)ptr;
    off_t posn;
   
    while ( (posn = lseek(fo->fd, 0, SEEK_CUR)) == -1) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    if (fo->flags & FILEOBJ_WRITE)  {
       posn += fo->bufWPtr;
    } else if (fo->flags & FILEOBJ_READ) {
       posn -= (fo->bufWPtr - fo->bufRPtr);
#if defined(_WIN32)
       if (!(fo->flags & FILEOBJ_BINARY)) {
	  /* Sigh, to get at the Real file position for files opened
	     in text mode, we need to scan the read buffer looking for
	     '\n's, making them count as \r\n (i.e., undoing the work of
             read()), since lseek() returns the raw position.
	  */
          int i, j;
	  i = fo->bufRPtr;
	  j = fo->bufWPtr;
          while (i <= j) {
	    if (((char*)fo->buf)[i] == '\n') {
	       posn--;
	    }
	    i++;
	  }
       }
#endif
    }
    return (StgInt)posn;
}

/* The following is only called with a position that we've already visited 
   (this is ensured by making the Haskell file posn. type abstract.)
*/
StgInt
setFilePosn(StgForeignPtr ptr, StgInt size, StgByteArray d)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int rc;
    off_t offset;

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

    rc = flushBuffer(ptr);
    if (rc < 0) return rc;

    while (lseek(fo->fd, offset, SEEK_SET) == -1) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    FILEOBJ_CLEAR_EOF(fo);
    return 0;
}
