/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: removeFile.c,v 1.4 2000/04/06 10:33:07 rrt Exp $
 *
 * removeFile Runtime Support
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
removeFile(StgByteArray path)
{
    struct stat sb;

    /* Check for a non-directory */
    while (stat(path, &sb) != 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    if (S_ISDIR(sb.st_mode)) {
	ghc_errtype = ERR_INAPPROPRIATETYPE;
	ghc_errstr = "file is a directory";
	return -1;
    }
    while (unlink(path) != 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    return 0;
}
