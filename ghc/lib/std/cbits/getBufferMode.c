/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: getBufferMode.c,v 1.1 1998/04/10 10:54:33 simonm Exp $
 *
 * hIs...Buffered Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

/*
 * We try to guess what the default buffer mode is going to be based 
 * on the type of file we're attached to.
 */

#define GBM_NB (0)
#define GBM_LB (-1)
#define GBM_BB (-2)
#define GBM_ERR (-3)

StgInt
getBufferMode(StgAddr fp)
{
    struct stat sb;

    /* Try to find out the file type */
    while (fstat(fileno((FILE *) fp), &sb) < 0) {
	/* highly unlikely */
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return GBM_ERR;
	}
    }
    /* Terminals are line-buffered by default */
    if (S_ISCHR(sb.st_mode) && isatty(fileno((FILE *) fp)) == 1)
	return GBM_LB;
    /* Default size block buffering for the others */
    else
	return GBM_BB;
}
