/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: closeFile.c,v 1.5 1999/07/03 18:45:04 sof Exp $
 *
 * hClose Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#if defined(HAVE_WINSOCK_H) && !defined(__CYGWIN__)
#define USE_WINSOCK
#endif

#ifdef USE_WINSOCK
#include <winsock.h>
#endif

StgInt __really_close_stdfiles=1;

StgInt
closeFile(ptr,flush_buf)
StgForeignPtr ptr;
StgInt flush_buf;
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int rc = 0;
    int unlocked=1;

    /* Already closed, shouldn't occur. */
    if ( fo == NULL ) {
       return 0;
    }

    if ( flush_buf != 0 && (fo->flags & FILEOBJ_FLUSH) ) {
       writeFileObject(ptr,fo->bufWPtr);
    }

    /* If the flush failed, we ignore this and soldier on.. */

    if ( unlockFile(fo->fd) ) {
      /* If the file has already been unlocked (or an entry
         for it in the locking tables couldn't be found), could
         mean two things:

	    - we're repeating an hClose on an already
	      closed file (this is likely to be a bug
	      in the implementation of hClose, as this 
	      condition should have been caught before
	      we ended up here.)
	      
	    - the file wasn't locked in the first place!
	      (file descriptors to non regular files.)

	 We proceed with attempting to close the file,
	 but don't flag the error should close() return
	 EBADF
      */
	unlocked=0;
	
    }

    /* Closing file descriptors that refer to standard channels
       is problematic, so we back off from doing this by default,
       just closing them at the Handle level. If you insist on
       closing them, setting the (global) variable 
       __really_close_stdfiles to 0 turns off this behaviour.
    */
    if ( (fo->flags & FILEOBJ_STD) && __really_close_stdfiles ) {
	;

    } else  {
      /* Regardless of success or otherwise, the fd field gets smashed. */
      while ( (rc = 
	        (
#ifdef USE_WINSOCK
	          fo->flags & FILEOBJ_WINSOCK ?
		  closesocket(fo->fd) :
                  close(fo->fd))) != 0 ) {
#else
                  close(fo->fd))) != 0 ) {
#endif
         /* See above unlockFile() comment */
	 if ( errno != EINTR && (!unlocked && errno != EBADF ) ) {
	    cvtErrno();
	    stdErrno();
	    fo->fd = -1;
	    return rc;
	}
      }
    }
    fo->fd = -1;
    return 0;
}
