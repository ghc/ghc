/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: freeFile.c,v 1.1 1998/04/10 10:54:31 simonm Exp $
 *
 * Giving up files
 */

#include "Rts.h"
#include "stgio.h"

/* sigh, the FILEs attached to the standard descriptors are 
   handled differently. We don't want them freed via the
   ForeignObj finaliser, as we probably want to use these
   before we *really* shut down (dumping stats etc.)
*/
void freeStdFile(StgAddr fp)
{ return; }

void freeFile(StgAddr fp)
{
    int rc;

    if ( fp == NULL || (rc = unlockFile(fileno((FILE *)fp))) ) {
	/* If the file handle has been explicitly closed
         * (via closeFile()) or freed, we will have given
	 * up our process lock, so we silently return here.
         */
       return;
    }

    /*
     * The finaliser for the FILEs embedded in Handles. The RTS
     * assumes that the finaliser runs without problems, so all
     * we can do here is fclose(), and hope nothing went wrong.
     *
     * Assume fclose() flushes output stream.
     */

    rc = fclose((FILE *)fp);
    /* Error or no error, we don't care.. */

    /* 
    if ( rc == EOF ) {
       fprintf(stderr. "Warning: file close ran into trouble\n");
    }
    */

    return;
}
