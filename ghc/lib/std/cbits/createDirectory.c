/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: createDirectory.c,v 1.4 1999/03/01 09:03:37 sof Exp $
 *
 * createDirectory Runtime Support}
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if defined(mingw32_TARGET_OS)
#define mkDir(nm,p) mkdir(nm)
#else
#define mkDir(nm,p) mkdir(nm,p)
#endif

StgInt 
createDirectory(path)
StgByteArray path;
{
    int rc;
    struct stat sb;

    while((rc = mkDir(path, 0777)) != 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    switch (ghc_errno) {
	    default:
		stdErrno();
		break;
	    case GHC_ENOENT:
	    case GHC_ENOTDIR:
		ghc_errtype = ERR_NOSUCHTHING;
		ghc_errstr = "no path to directory";
		break;
	    case GHC_EEXIST:
		if (stat(path, &sb) != 0) {
		    ghc_errtype = ERR_OTHERERROR;
		    ghc_errstr = "cannot stat existing file";
		} 
		if (S_ISDIR(sb.st_mode)) {
		    ghc_errtype = ERR_ALREADYEXISTS;
		    ghc_errstr = "directory already exists";
		} else {
		    ghc_errtype = ERR_INAPPROPRIATETYPE;
		    ghc_errstr = "file already exists";
		}
		break;
	    }
	    return -1;
	}
    }
    return 0;
}
