/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: fileObject.c,v 1.6 1999/09/16 13:14:42 simonmar Exp $
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
setBufFlags(fo, flg)
StgForeignPtr fo;
StgInt flg;
{
  ((IOFileObject*)fo)->flags = flg;
  return;
}

void
setBufWPtr(fo, len)
StgForeignPtr fo;
StgInt len;
{
  ((IOFileObject*)fo)->bufWPtr = len;
  return;
}

StgInt
getBufWPtr(fo)
StgForeignPtr fo;
{
  return (((IOFileObject*)fo)->bufWPtr);
}

StgInt
getBufSize(fo)
StgForeignPtr fo;
{
  return (((IOFileObject*)fo)->bufSize);
}

void
setBuf(fo, buf,sz)
StgForeignPtr fo;
StgAddr buf;
StgInt sz;
{
  ((IOFileObject*)fo)->buf     = buf;
  ((IOFileObject*)fo)->bufSize = sz;
  return;
}

StgAddr
getBuf(fo)
StgForeignPtr fo;
{ return (((IOFileObject*)fo)->buf); }

StgAddr
getWriteableBuf(ptr)
StgForeignPtr ptr;
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
getBufStart(fo,count)
StgForeignPtr fo;
StgInt count;
{ return ((char*)((IOFileObject*)fo)->buf + (((IOFileObject*)fo)->bufRPtr) - count); }

StgInt
getFileFd(fo)
StgForeignPtr fo;
{ return (((IOFileObject*)fo)->fd); }

StgInt
getConnFileFd(fo)
StgForeignPtr fo;
{ return (((IOFileObject*)fo)->connectedTo->fd); }


void
setFd(fo,fp)
StgForeignPtr fo;
StgInt fp;
{ ((IOFileObject*)fo)->fd = fp;
  return;
}

void
setConnectedTo(fo, fw, flg)
StgForeignPtr fo;
StgForeignPtr fw;
StgInt flg;
{
  if( flg && (! isatty(((IOFileObject*)fo)->fd) || !isatty(((IOFileObject*)fw)->fd)) ) {
      return;
  }
 ((IOFileObject*)fo)->connectedTo = (IOFileObject*)fw;
  return;
}

static int __pushback_buf_size__ = 2;

void
setPushbackBufSize(i)
StgInt i;
{ __pushback_buf_size__ = (i > 0 ? i : 0); }

StgInt
getPushbackBufSize()
{ return (__pushback_buf_size__); }

/* Only ever called on line-buffered file objects */
StgInt
fill_up_line_buffer(fo)
IOFileObject* fo;
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
