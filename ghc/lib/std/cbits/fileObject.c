/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: fileObject.c,v 1.7 1999/11/23 14:38:40 simonmar Exp $
 *
 * hPutStr Runtime Support
 */

#include "Rts.h"
#include "stgio.h"
#include "fileObject.h"

#include <stdio.h>

#if defined(HAVE_WINSOCK_H) && !defined(__CYGWIN__) && !defined(__CYGWIN32__)
#define USE_WINSOCK
#endif

#ifdef USE_WINSOCK
#include <winsock.h>
#endif

void
setBufFlags(StgForeignPtr fo, StgInt flg)
{
  ((IOFileObject*)fo)->flags = flg;
  return;
}

void
setBufWPtr(StgForeignPtr fo, StgInt len)
{
  ((IOFileObject*)fo)->bufWPtr = len;
  return;
}

StgInt
getBufWPtr(StgForeignPtr fo)
{
  return (((IOFileObject*)fo)->bufWPtr);
}

StgInt
getBufSize(StgForeignPtr fo)
{
  return (((IOFileObject*)fo)->bufSize);
}

void
setBuf(StgForeignPtr fo, StgAddr buf,StgInt sz)
{
  ((IOFileObject*)fo)->buf     = buf;
  ((IOFileObject*)fo)->bufSize = sz;
  return;
}

StgAddr
getBuf(StgForeignPtr fo)
{ return (((IOFileObject*)fo)->buf); }

StgAddr
getWriteableBuf(StgForeignPtr ptr)
{ 
   /* getWriteableBuf() is called prior to starting to pack
      a Haskell string into the IOFileObject buffer. It takes
      care of flushing the (input) buffer in the case we're
      dealing with a RW handle.
   */
   IOFileObject* fo = (IOFileObject*)ptr;

   if ( FILEOBJ_READABLE(fo) && FILEOBJ_JUST_READ(fo) ) {
      flushReadBuffer(ptr);  /* ignoring return code */
      /* Ahead of time really, but indicate that we're (just about to) write */
   }
   fo->flags = (fo->flags & ~FILEOBJ_RW_READ) | FILEOBJ_RW_WRITE;
   return (fo->buf);
}

StgAddr
getBufStart(StgForeignPtr fo, StgInt count)
{ return ((char*)((IOFileObject*)fo)->buf + (((IOFileObject*)fo)->bufRPtr) - count); }

StgInt
getFileFd(StgForeignPtr fo)
{ return (((IOFileObject*)fo)->fd); }

StgInt
getConnFileFd(StgForeignPtr fo)
{ return (((IOFileObject*)fo)->connectedTo->fd); }


void
setFd(StgForeignPtr fo,StgInt fp)
{ ((IOFileObject*)fo)->fd = fp;
  return;
}

void
setConnectedTo(StgForeignPtr fo, StgForeignPtr fw, StgInt flg)
{
  if( flg && (! isatty(((IOFileObject*)fo)->fd) || !isatty(((IOFileObject*)fw)->fd)) ) {
      return;
  }
 ((IOFileObject*)fo)->connectedTo = (IOFileObject*)fw;
  return;
}

static int __pushback_buf_size__ = 2;

void
setPushbackBufSize(StgInt i)
{ __pushback_buf_size__ = (i > 0 ? i : 0); }

StgInt
getPushbackBufSize(void)
{ return (__pushback_buf_size__); }

/* Only ever called on line-buffered file objects */
StgInt
fill_up_line_buffer(IOFileObject* fo)
{
  int count,len, ipos;
  unsigned char* p;

  /* ToDo: deal with buffer overflow (i.e., realloc buffer if this happens) */
 
  if ( fo->bufRPtr == fo->bufWPtr ) { /* There's nothing in the buffer, reset */
      fo->bufRPtr=0;
      fo->bufWPtr=0;
  }
  ipos = fo->bufWPtr;
  len = fo->bufSize - fo->bufWPtr + 1;
  p   = (unsigned char*)fo->buf + fo->bufWPtr;

  if ((count = 
         (
#ifdef USE_WINSOCK
	   fo->flags & FILEOBJ_WINSOCK ?
	   recv(fo->fd, p, len, 0) :
	   read(fo->fd, p, len))) <= 0 ) {
#else
	   read(fo->fd, p, len))) <= 0 ) {
#endif    
      if (count == 0) {
         ghc_errtype = ERR_EOF;
	 ghc_errstr = "";
         FILEOBJ_SET_EOF(fo);
	 return -1;
      } else if ( count == -1 && errno == EAGAIN) {
	 errno = 0;
	 return FILEOBJ_BLOCKED_READ;
      } else if ( count == -1 && errno != EINTR ) {
         cvtErrno();
	 stdErrno();
	 return -1;
      }
  }
  fo->bufWPtr += count;
  return (fo->bufWPtr - ipos);
}
