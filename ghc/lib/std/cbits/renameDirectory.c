/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: renameDirectory.c,v 1.3 1998/12/02 13:27:50 simonm Exp $
 *
 * renameDirectory Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

StgInt
renameDirectory(opath, npath)
StgByteArray opath;
StgByteArray npath;
{
    struct stat sb;

    /* Check for an actual directory */
    while (stat(opath, &sb) != 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    if (!S_ISDIR(sb.st_mode)) {
	ghc_errtype = ERR_INAPPROPRIATETYPE;
	ghc_errstr = "not a directory";
	return -1;
    }
    while(rename(opath, npath) != 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    return 0;
}
