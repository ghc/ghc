/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: readFile.c,v 1.15 2000/04/12 17:33:16 simonmar Exp $
 *
 * hGetContents Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#if defined(HAVE_WINSOCK_H) && !defined(__CYGWIN__) && !defined(__CYGWIN32__)
#define USE_WINSOCK
#endif

#ifdef USE_WINSOCK
#include <winsock.h>
#endif

#define EOT 4

/* Filling up a (block-buffered) buffer, that
   is completely empty. */
StgInt
readBlock(StgForeignPtr ptr)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int count,rc=0;
    int fd;

    /* Check if someone hasn't zapped us */
    if ( fo == NULL || fo->fd == -1 )
       return -2;

    fd = fo->fd;

    if ( FILEOBJ_IS_EOF(fo) ) {
	ghc_errtype = ERR_EOF;
	ghc_errstr = "";
	return -1;
    }

    /* Weird case: buffering has suddenly been turned off.
       Return non-std value and deal with this case on the Haskell side.
    */
    if ( FILEOBJ_UNBUFFERED(fo) ) {
        return -3;
    }

    /* if input stream is connect to an output stream, flush this one first. */
    if ( fo->connectedTo != NULL   &&
         fo->connectedTo->fd != -1 &&
         (fo->connectedTo->flags & FILEOBJ_WRITE)
       ) {
       rc = flushFile((StgForeignPtr)fo->connectedTo);
    }
    if (rc < 0) return (rc == FILEOBJ_BLOCKED_WRITE ? FILEOBJ_BLOCKED_CONN_WRITE : rc);

    /* RW object: flush the (output) buffer first. */
    if ( FILEOBJ_WRITEABLE(fo) && FILEOBJ_JUST_WRITTEN(fo) && FILEOBJ_NEEDS_FLUSHING(fo) ) {
        rc = flushBuffer(ptr);
	if (rc < 0) return rc;
    }
    fo->flags = (fo->flags & ~FILEOBJ_RW_WRITE) | FILEOBJ_RW_READ;

    /* return the unread parts of the file buffer..*/
    if ( fo->flags & FILEOBJ_READ && 
    	 fo->bufRPtr > 0 	  &&
	 fo->bufWPtr > fo->bufRPtr ) {
	count = fo->bufWPtr - fo->bufRPtr;
        fo->bufRPtr=0;
        return count;
    }

#if 0
    fprintf(stderr, "rb: %d %d %d\n", fo->bufRPtr, fo->bufWPtr, fo->bufSize);
#endif

    while ((count =
	     (
#ifdef USE_WINSOCK
	       fo->flags & FILEOBJ_WINSOCK ?
	         recv(fd, fo->buf, fo->bufSize, 0) :
	         read(fd, fo->buf, fo->bufSize))) <= 0 ) {
#else
	         read(fd, fo->buf, fo->bufSize))) <= 0 ) {
#endif
	if ( count == 0 ) {
            FILEOBJ_SET_EOF(fo);
	    ghc_errtype = ERR_EOF;
	    ghc_errstr = "";
	    return -1;
	} else if ( count == -1 && errno == EAGAIN) {
	    errno = 0;
	    return FILEOBJ_BLOCKED_READ;
	} else if ( count == -1 && errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    fo->bufWPtr = count;
    fo->bufRPtr = 0;
    return count;
}

/* Filling up a (block-buffered) buffer of length len */

/* readChunk(FileObjet *, void *, int)
 * returns:
 *  -1                             error
 *  -2                             object closed
 *  FILEOBJ_BLOCKED_CONN_WRITE     blocking while flushing
 *                                 buffer of connected handle.
 *  FILEOBJ_BLOCKED_READ           didn't read anything; would block
 *  n, where n > 0                 read n bytes into buffer.
 *  0				   EOF has been reached
 */

StgInt
readChunk(StgForeignPtr ptr, StgAddr buf, StgInt off, StgInt len)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int count, rc=0, total_count=0;
    int fd;
    char* p;

    /* Check if someone hasn't zapped us */
    if ( fo == NULL )
       return -2;

    fd = fo->fd;

    if ( fd == -1 ) /* File has been closed for us */
       return -2;

    if ( FILEOBJ_IS_EOF(fo) ) {
        return 0;
    }

    /* if input stream is connect to an output stream, flush it first */
    if ( fo->connectedTo != NULL   &&
         fo->connectedTo->fd != -1 &&
         (fo->connectedTo->flags & FILEOBJ_WRITE)
       ) {
       rc = flushFile((StgForeignPtr)fo->connectedTo);
    }
    if (rc < 0) return (rc == FILEOBJ_BLOCKED_WRITE ? FILEOBJ_BLOCKED_CONN_WRITE : rc);

    /* RW object: flush the (output) buffer first. */
    if ( FILEOBJ_WRITEABLE(fo) && FILEOBJ_JUST_WRITTEN(fo) && FILEOBJ_NEEDS_FLUSHING(fo) ) {
        rc = flushBuffer(ptr);
	if (rc < 0) return rc;
    }
    fo->flags = (fo->flags & ~FILEOBJ_RW_WRITE) | FILEOBJ_RW_READ;

    p = buf+off;

    /* copy the unread parts of the file buffer..*/
    if ( FILEOBJ_READABLE(fo) && 
    	 fo->bufRPtr > 0      &&
	 fo->bufWPtr >= fo->bufRPtr ) {

        if (fo->bufWPtr - fo->bufRPtr >= len) {
            /* buffer has enough data to fulfill the request */
	    memcpy(buf, fo->buf + fo->bufRPtr, len);
            fo->bufRPtr += len;
            return len;
        } else {
            /* can only partially fulfill the request from the buffer */
            count = fo->bufWPtr - fo->bufRPtr;
	    memcpy(buf, fo->buf + fo->bufRPtr, count);
            fo->bufWPtr=0;
	    fo->bufRPtr=0;
            len -= count;
            p += count;
            total_count = count;
        }
    }

    while ((count =
             (
#ifdef USE_WINSOCK
	       fo->flags & FILEOBJ_WINSOCK ?
	         recv(fd, p, len, 0) :
	         read(fd, p, len))) <= 0 ) {
#else
	         read(fd, p, len))) <= 0 ) {
#endif
        /* EOF */
	if ( count == 0 ) {
            FILEOBJ_SET_EOF(fo);
            return total_count;
	}

        /* Blocking */
	else if ( count == -1 && errno == EAGAIN) {
	    errno = 0;
            if (total_count > 0) 
               return total_count; /* partial read */
	    else
	       return FILEOBJ_BLOCKED_READ;
	}

        /* Error */
	else if ( count == -1 && errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
        }
    }

    total_count += count;
    return total_count;
}

/*
  readLine() tries to fill the buffer up with a line of chars, returning
  the length of the resulting line. 
  
  Users of readLine() should immediately afterwards copy out the line
  from the buffer.

*/

StgInt
readLine(StgForeignPtr ptr)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int rc=0, count;

    /* Check if someone hasn't zapped us */
    if ( fo == NULL || fo->fd == -1 )
       return -2;

    if ( FILEOBJ_IS_EOF(fo) ) {
	ghc_errtype = ERR_EOF;
	ghc_errstr = "";
	return -1;
    }

    /* Weird case: buffering has been turned off.
       Return non-std value and deal with this case on the Haskell side.
    */
    if ( FILEOBJ_UNBUFFERED(fo) ) {
        return -3;
    }

    /* if input stream is connect to an output stream, flush it first */
    if ( fo->connectedTo != NULL   &&
         fo->connectedTo->fd != -1 &&
         (fo->connectedTo->flags & FILEOBJ_WRITE)
       ) {
       rc = flushFile((StgForeignPtr)fo->connectedTo);
    }
    if (rc < 0) return (rc == FILEOBJ_BLOCKED_WRITE ? FILEOBJ_BLOCKED_CONN_WRITE : rc);

    /* RW object: flush the (output) buffer first. */
    if ( FILEOBJ_WRITEABLE(fo) && FILEOBJ_JUST_WRITTEN(fo) ) {
        rc = flushBuffer(ptr);
	if (rc < 0) return rc;
    }
    fo->flags = (fo->flags & ~FILEOBJ_RW_WRITE) | FILEOBJ_RW_READ;

    if ( fo->bufRPtr < 0 || fo->bufRPtr >= fo->bufWPtr ) { /* Buffer is empty */
        fo->bufRPtr=0; 
	fo->bufWPtr=0;
        rc = fill_up_line_buffer(fo);
        if (rc < 0) return rc;
    }

    while (1) {
       unsigned char* s1 = memchr((unsigned char *)fo->buf+fo->bufRPtr, '\n', fo->bufWPtr - fo->bufRPtr);
       if (s1 != NULL ) {  /* Found one */
	  /* Note: we *don't* zero terminate the line */
	  count = s1 - ((unsigned char*)fo->buf + fo->bufRPtr) + 1;
	  fo->bufRPtr += count;
          return count;
       } else {
          /* Just return partial line */
	  count = fo->bufWPtr - fo->bufRPtr;
	  fo->bufRPtr += count;
          return count;
       }
    }

}

StgInt
readChar(StgForeignPtr ptr)
{
    IOFileObject* fo= (IOFileObject*)ptr;
    int count,rc=0;
    unsigned char c;

    /* Check if someone hasn't zapped us */
    if ( fo == NULL || fo->fd == -1)
       return -2;

    if ( FILEOBJ_IS_EOF(fo) ) {
	ghc_errtype = ERR_EOF;
	ghc_errstr = "";
	return -1;
    }

    /* Buffering has been changed, report back */
    if ( FILEOBJ_LINEBUFFERED(fo) ) {
       return -3;
    } else if ( FILEOBJ_BLOCKBUFFERED(fo) ) {
       return -4;
    }

    /* if input stream is connect to an output stream, flush it first */
    if ( fo->connectedTo != NULL   &&
         fo->connectedTo->fd != -1 &&
         (fo->connectedTo->flags & FILEOBJ_WRITE)
       ) {
       rc = flushFile((StgForeignPtr)fo->connectedTo);
    }
    if (rc < 0) return (rc == FILEOBJ_BLOCKED_WRITE ? FILEOBJ_BLOCKED_CONN_WRITE : rc);

    /* RW object: flush the (output) buffer first. */
    if ( FILEOBJ_WRITEABLE(fo) && FILEOBJ_JUST_WRITTEN(fo) && FILEOBJ_NEEDS_FLUSHING(fo) ) {
        rc = flushBuffer(ptr);
	if (rc < 0) return rc;
    }
    fo->flags = (fo->flags & ~FILEOBJ_RW_WRITE) | FILEOBJ_RW_READ;

    while ( (count = 
	       (
#ifdef USE_WINSOCK
	         fo->flags & FILEOBJ_WINSOCK ?
		 recv(fo->fd, &c, 1, 0) :
		 read(fo->fd, &c, 1))) <= 0 ) {
#else
		 read(fo->fd, &c, 1))) <= 0 ) {
#endif
	if ( count == 0 ) {
	    ghc_errtype = ERR_EOF;
	    ghc_errstr = "";
	    return -1;
	} else if ( count == -1 && errno == EAGAIN) {
	    errno = 0;
	    return FILEOBJ_BLOCKED_READ;
	} else if ( count == -1 && errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }

    if ( isatty(fo->fd) && c == EOT ) {
	return EOF;
    } else {
        return (int)c;
    }
}
