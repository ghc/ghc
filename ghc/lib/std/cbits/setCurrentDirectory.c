/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: setCurrentDirectory.c,v 1.1 1998/04/10 10:54:52 simonm Exp $
 *
 * setCurrentDirectory Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

StgInt
setCurrentDirectory(StgByteArray path)
{
    while (chdir(path) != 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    return 0;
}
