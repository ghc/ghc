/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: freeFile.c,v 1.3 1998/12/02 13:27:34 simonm Exp $
 *
 * Giving up files
 */

#include "Rts.h"
#include "stgio.h"
#include "fileObject.h"

/* sigh, the FILEs attached to the standard descriptors are 
   handled differently. We don't want them freed via the
   ForeignObj finaliser, as we probably want to use these
   before we *really* shut down (dumping stats etc.)
*/
void freeStdFile(fp)
StgForeignPtr fp;
{ return; }

void freeStdFileObject(ptr)
StgForeignPtr ptr;
{ 
  IOFileObject* fo = (IOFileObject*)ptr;

  /* Don't close the file, just flush the buffer */
  if (fo != NULL && fo->fd != -1) {
    if (fo->buf != NULL && (fo->flags & FILEOBJ_FLUSH) && fo->bufWPtr > 0) {
       /* Flush buffer contents */
       writeBuffer((StgForeignPtr)fo, fo->bufWPtr);
    }
  }
}

void freeFileObject(ptr)
StgForeignPtr ptr;
{
    /*
     * The finaliser for the file objects embedded in Handles. The RTS
     * assumes that the finaliser runs without problems, so all
     * we can do here is flish buffers + close(), and hope nothing went wrong.
     *
     */

    int rc;
    IOFileObject* fo = (IOFileObject*)ptr;

    if ( fo == NULL )
      return;

    if ( fo->fd == -1 || (rc = unlockFile(fo->fd)) ) {
	/* If the file handle has been explicitly closed
         * (via closeFile()), we will have given
	 * up our process lock, so we break off and just return.
         */
       return;
    }

    if (fo->buf != NULL && fo->bufWPtr > 0) {
       /* Flush buffer contents before closing underlying file */
       fo->flags &= ~FILEOBJ_RW_WRITE | ~FILEOBJ_RW_READ;
       flushFile(ptr);
    }

    rc = close(fo->fd);
    /* Error or no error, we don't care.. */

    return;
}

StgAddr	ref_freeStdFileObject(void)
{
    return (StgAddr)&freeStdFileObject;
}

StgAddr	ref_freeFileObject(void)
{
    return (StgAddr)&freeFileObject;
}

