/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: filePosn.c,v 1.1 1998/04/10 10:54:25 simonm Exp $
 *
 * hGetPosn and hSetPosn Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

StgInt
getFilePosn(StgAddr fp)
{
    StgInt posn;

    while ((posn = ftell((FILE *) fp)) == -1) {
	/* the possibility seems awfully remote */
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    return posn;
}

/* The following is only called with a position that we've already visited */

StgInt
setFilePosn(StgAddr fp, I_ posn)
{
    while (fseek((FILE *) fp, posn, SEEK_SET) != 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    return 0;
}


