/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: writeFile.c,v 1.14 2000/04/12 17:33:16 simonmar Exp $
 *
 * hPutStr Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#if defined(HAVE_WINSOCK_H) && !defined(__CYGWIN__) && !defined(__CYGWIN32__)
#define USE_WINSOCK
#endif

#ifdef USE_WINSOCK
#include <winsock.h>
#endif

StgInt
writeFileObject(StgForeignPtr ptr, StgInt bytes)
{
    int rc=0;
    IOFileObject* fo = (IOFileObject*)ptr;

    /* If we've got a r/w file object in our hand, flush the
       (input) buffer contents first.
    */
    if ( FILEOBJ_READABLE(fo) && FILEOBJ_JUST_READ(fo) ) {
       fo->flags = (fo->flags & ~FILEOBJ_RW_READ) | FILEOBJ_RW_WRITE;
       rc = flushReadBuffer(ptr);
       if (rc < 0) return rc;
    }

    return (writeBuffer(ptr, bytes));
}

StgInt
writeBuffer(StgForeignPtr ptr, StgInt bytes)
{
    int count;
    IOFileObject* fo = (IOFileObject*)ptr;

    char *pBuf = (char *) fo->buf + fo->bufRPtr;

    bytes -= fo->bufRPtr;

    /* Disallow short writes */
    if (bytes == 0  || fo->buf == NULL) {
        fo->bufRPtr = 0;
	return 0;
    }

    while ((count = 
	       (
#ifdef USE_WINSOCK
	         fo->flags & FILEOBJ_WINSOCK ?
		 send(fo->fd,  pBuf, bytes, 0) :
		 write(fo->fd, pBuf, bytes))) < bytes) {
#else
		 write(fo->fd, pBuf, bytes))) < bytes) {
#endif
        if ( count == -1 && errno == EAGAIN) {
            errno = 0;
            return FILEOBJ_BLOCKED_WRITE;
        }
	else if ( count == -1 && errno != EINTR ) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
        else {
	    bytes -= count;
	    pBuf  += count;
            fo->bufRPtr += count;
        }
    }
    /* Signal that we've emptied the buffer */
    fo->bufRPtr = 0;
    fo->bufWPtr = 0;
    return 0;
}


/* ToDo: there's currently no way for writeBuf to return both a
 * partial write and an indication that the write blocked.  It needs
 * two calls: one to get the partial result, and the next one to block.
 * This matches Unix write/2, but is rather a waste.
 */

StgInt
writeBuf(StgForeignPtr ptr, StgAddr buf, StgInt off, StgInt len)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int count, total_count;
    int rc = 0;
    char *pBuf = (char *) buf+off;

    if (len == 0)
	return 0;

    /* First of all, check if we do need to flush the buffer .. */
    /* Note - in the case of line buffering, we do not currently check
       whether we need to flush buffer due to line terminators in the
       buffer we're outputting */
    if ( fo->buf != NULL  		     &&   /* buffered and */
         (fo->bufWPtr + len < (fo->bufSize))      /* there's room */
       ) {
       /* Block copying is likely to be cheaper than flush, followed by write */
       memcpy(((char*)fo->buf + fo->bufWPtr), pBuf, len);
       fo->bufWPtr += len;
       return len;
    }
    /* If we do overflow, flush current contents of the buffer and
       directly output the chunk.
       (no attempt at splitting up the chunk is currently made)
    */       
    if ( fo->buf != NULL  		     &&    /* buffered and */
         (fo->bufWPtr + len >= (fo->bufSize))       /* there's not room */
       ) {
       /* Flush buffer */
       rc = writeFileObject(ptr, fo->bufWPtr);
       /* ToDo: undo buffer fill if we're blocking.. */
       if (rc != 0) { 
           return rc;
       }
    }

    total_count = 0;

    while ((count = 
               (
#ifdef USE_WINSOCK
	         fo->flags & FILEOBJ_WINSOCK ?
		 send(fo->fd,  pBuf, (int)len, 0) :
		 write(fo->fd, pBuf, (int)len))) < len ) {
#else
		 write(fo->fd, pBuf, (int)len))) < len ) {
#endif
        if ( count >= 0 ) {
            len -= count;
	    pBuf += count;
            total_count += count;
	    continue;
	} else if ( errno == EAGAIN ) {
	    errno = 0;
            if (total_count > 0)
                return total_count; /* partial write */
 	    else
		return FILEOBJ_BLOCKED_WRITE;
	} else if ( errno != EINTR ) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }

    total_count += count;
    return total_count;
}

StgInt
writeBufBA(StgForeignPtr ptr, StgByteArray buf, StgInt off, StgInt len)
{ 
    return (writeBuf(ptr,(StgAddr)buf, off, len)); 
}

/* -----------------------------------------------------------------------------
 * write_  is just a simple wrapper around write/2 that restarts
 * on EINTR and returns FILEOBJ_BLOCKED_WRITE on EAGAIN.
 * -------------------------------------------------------------------------- */

StgInt
write_(StgForeignPtr ptr, StgAddr buf, StgInt len)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int rc;

    while ((rc = 
               (
#ifdef USE_WINSOCK
	         fo->flags & FILEOBJ_WINSOCK ?
		 send(fo->fd,  buf, (int)len, 0) :
		 write(fo->fd, buf, (int)len))) < 0 ) {
#else
		 write(fo->fd, buf, (int)len))) < 0 ) {
#endif
	if ( errno == EAGAIN ) {
            errno = 0;
            return FILEOBJ_BLOCKED_WRITE;
	} else if ( errno != EINTR ) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    return rc;
}  
