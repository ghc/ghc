/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: writeFile.c,v 1.9 1999/11/05 15:25:49 simonmar Exp $
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
writeFileObject(ptr, bytes)
StgForeignPtr ptr;
StgInt bytes;
{
    int rc=0;
    IOFileObject* fo = (IOFileObject*)ptr;

    char *p = (char *) fo->buf;

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
writeBuffer(ptr, bytes)
StgForeignPtr ptr;
StgInt bytes;
{
    int count, rc=0;
    IOFileObject* fo = (IOFileObject*)ptr;

    char *pBuf = (char *) fo->buf;

    /* Disallow short writes */
    if (bytes == 0  || fo->buf == NULL)
	return 0;

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
        }
    }
    /* Signal that we've emptied the buffer */
    fo->bufWPtr=0;
    return 0;
}


StgInt
writeBuf(ptr, buf, len)
StgForeignPtr ptr;
StgAddr buf;
StgInt  len;
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int count;
    int rc = 0;
    char *pBuf = (char *) buf;

    if (len == 0 )
	return 0;

    /* First of all, check if we do need to flush the buffer .. */
    /* Note - in the case of line buffering, we do not currently check
       whether we need to flush buffer due to line terminators in the
       buffer we're outputting */
    if ( fo->buf != NULL  		     &&   /* buffered and */
         (fo->bufWPtr + len < (fo->bufSize))      /* there's room */
       ) {
       /* Block copying is likely to be cheaper than, flush, followed by write */
       memcpy(((char*)fo->buf + fo->bufWPtr), buf, len);
       fo->bufWPtr += len;
       return 0;
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
    }

    if (rc != 0) { 
       return rc;
    }

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
	    continue;
	} else if ( errno == EAGAIN ) {
	    errno = 0;
	    return FILEOBJ_BLOCKED_WRITE;
	} else if ( errno != EINTR ) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }

    return 0;
}

StgInt
writeBufBA(ptr, buf, len)
     StgForeignPtr ptr;
StgByteArray buf;
StgInt  len;
{ return (writeBuf(ptr,(StgAddr)buf, len)); }
