/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: writeFile.c,v 1.6 1999/07/12 10:43:13 sof Exp $
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

    char *p = (char *) fo->buf;

    /* Disallow short writes */
    if (bytes == 0  || fo->buf == NULL)
	return 0;

    if ( fo->flags & FILEOBJ_NONBLOCKING_IO && inputReady(ptr,0) != 1 )
       return FILEOBJ_BLOCKED_WRITE;

    while ((count = 
	       (
#ifdef USE_WINSOCK
	         fo->flags & FILEOBJ_WINSOCK ?
		 send(fo->fd, fo->buf, bytes, 0) :
		 write(fo->fd, fo->buf, bytes))) < bytes) {
#else
		 write(fo->fd, fo->buf, bytes))) < bytes) {
#endif
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
	bytes -= count;
	p += count;
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
    char *p = (char *) buf;

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

    if ( fo->flags & FILEOBJ_NONBLOCKING_IO && inputReady(ptr,0) != 1 )
       return FILEOBJ_BLOCKED_WRITE;

    /* Disallow short writes */
    while ((count = 
               (
#ifdef USE_WINSOCK
	         fo->flags & FILEOBJ_WINSOCK ?
		 send(fo->fd,  (char*)buf, (int)len, 0) :
		 write(fo->fd, (char*)buf, (int)len))) < len ) {
#else
		 write(fo->fd, (char*)buf, (int)len))) < len ) {
#endif
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
	len -= count;
	p += count;
    }

    return 0;
}

StgInt
writeBufBA(ptr, buf, len)
     StgForeignPtr ptr;
StgByteArray buf;
StgInt  len;
{ return (writeBuf(ptr,(StgAddr)buf, len)); }
