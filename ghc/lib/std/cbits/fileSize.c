/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: fileSize.c,v 1.6 2001/03/01 12:25:33 rrt Exp $
 *
 * hClose Runtime Support
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
fileSize(StgForeignPtr ptr, StgByteArray result)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    struct Stat sb;
    int rc = 0;

    /* Flush buffer in order to get as an accurate size as poss. */
    rc = flushFile(ptr);
    if (rc < 0) return rc;

   while (Fstat(fo->fd, &sb) < 0) {
	/* highly unlikely */
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    if (S_ISREG(sb.st_mode)) {
	/* result will be word aligned */
#if defined( macosx_TARGET_OS )
	*(W_ *) result = (W_)sb.st_size;
#else
	*(off_t *) result = sb.st_size;
#endif
	return 0;
    } else {
	ghc_errtype = ERR_INAPPROPRIATETYPE;
	ghc_errstr = "not a regular file";
	return -1;
    }
}

StgInt
fileSize_int64(StgForeignPtr ptr, StgByteArray result)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    struct Stat sb;
    int rc = 0;

    /* Flush buffer in order to get as an accurate size as poss. */
    rc = flushFile(ptr);
    if (rc < 0) return rc;

   while (Fstat(fo->fd, &sb) < 0) {
	/* highly unlikely */
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }
    if (S_ISREG(sb.st_mode)) {
	/* result will be word aligned */
	*(StgInt64*) result = (StgInt64)sb.st_size;
	return 0;
    } else {
	ghc_errtype = ERR_INAPPROPRIATETYPE;
	ghc_errstr = "not a regular file";
	return -1;
    }
}

