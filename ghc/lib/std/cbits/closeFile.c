/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: closeFile.c,v 1.1 1998/04/10 10:54:14 simonm Exp $
 *
 * hClose Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

StgInt
closeFile(StgAddr fp)
{
    int rc;

    if (unlockFile(fileno((FILE *) fp))) {
       /* If it has been unlocked, don't bother fclose()ing */
       return 0;
    }

    while ((rc = fclose((FILE *) fp)) != 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return rc;
	}
    }
    return 0;
}



