/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: flushFile.c,v 1.6 1999/11/25 16:54:14 simonmar Exp $
 *
 * hFlush Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

StgInt
flushFile(StgForeignPtr ptr)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int rc = 0;

    if ( (fo->flags & FILEOBJ_WRITE) && FILEOBJ_NEEDS_FLUSHING(fo) ) {
       rc = writeBuffer(ptr,fo->bufWPtr - fo->bufRPtr);
    }

    return rc;
}

StgInt
flushBuffer(StgForeignPtr ptr)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int rc = 0;

    /* If the file object is writeable, or if it's
       RW *and* the last operation on it was a write,
       flush it.
    */
    if ( (!FILEOBJ_READABLE(fo) && FILEOBJ_WRITEABLE(fo)) ||
         (FILEOBJ_RW(fo) && FILEOBJ_JUST_WRITTEN(fo)) ) {
       rc = flushFile(ptr);
       if (rc<0) return rc;
    }
    
    /* Reset read & write pointer for input buffers */
    if ( (fo->flags & FILEOBJ_READ) ) {
       fo->bufRPtr=0;
       fo->bufWPtr=0;
    }
    return 0;
}

/*
 For RW file objects, flushing input buffers doesn't just involve 
 resetting the read & write pointers, we also have to change the
 underlying file position to point to the effective read position.

 (Sigh, I now understand the real reason for why stdio opted for
 the solution of leaving this to the programmer!)
*/
StgInt
flushReadBuffer(StgForeignPtr ptr)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int delta;

    delta = fo->bufWPtr - fo->bufRPtr;

    if ( delta > 0 ) {
       while ( lseek(fo->fd, -delta, SEEK_CUR) == -1) {
	  if (errno != EINTR) {
	     cvtErrno();
	     stdErrno();
	     return -1;
	  }
       }
    }

    fo->bufRPtr=0;
    fo->bufWPtr=0;
    return 0;
}

void
flushConnectedBuf(StgForeignPtr ptr)
{
    StgInt rc;
    IOFileObject* fo = (IOFileObject*)ptr;
    
    /* if the stream is connected to an output stream, flush it. */
    if ( fo->connectedTo != NULL && fo->connectedTo->fd != -1 &&
         (fo->connectedTo->flags & FILEOBJ_WRITE) ) {
       rc = flushBuffer((StgForeignPtr)fo->connectedTo);
    }
    /* Willfully ignore the return code for now. */
    return;
}

  
