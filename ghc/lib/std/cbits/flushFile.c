/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: flushFile.c,v 1.1 1998/04/10 10:54:30 simonm Exp $
 *
 * hFlush Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

StgInt
flushFile(StgAddr fp)
{
    int rc;

    while ((rc = fflush((FILE *) fp)) != 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return rc;
	}
    }
    return 0;
}
