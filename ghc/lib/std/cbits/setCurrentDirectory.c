/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: setCurrentDirectory.c,v 1.3 1998/12/02 13:27:56 simonm Exp $
 *
 * setCurrentDirectory Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

StgInt
setCurrentDirectory(path)
StgByteArray path;
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
