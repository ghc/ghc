/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: renameFile.c,v 1.7 2000/04/06 10:26:09 rrt Exp $
 *
 * renameFile Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif


StgInt
renameFile(opath, npath)
StgByteArray opath;
StgByteArray npath;
{
    struct stat sb;

    /* Check for a non-directory source */
    while (stat(opath, &sb) != 0) {
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

    /* Check for a non-directory destination */
    while (stat(npath, &sb) != 0 && errno != ENOENT) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }

    if (errno != ENOENT) {
        if (S_ISDIR(sb.st_mode)) {
	    ghc_errtype = ERR_INAPPROPRIATETYPE;
	    ghc_errstr = "file is a directory";
	    return -1;
        }
        while (chmod(npath, S_IWUSR) != 0) {
	    if (errno != EINTR) {
                cvtErrno();
                stdErrno();
                return -1;
            }
	}
        while (unlink(npath) != 0) {
            if (errno != EINTR) {
                cvtErrno();
                stdErrno();
                return -1;
	    }
	}
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
