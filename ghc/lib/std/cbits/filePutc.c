/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: filePutc.c,v 1.1 1998/04/10 10:54:26 simonm Exp $
 *
 * hPutChar Runtime Support
 */

#include "Rts.h"
#include "stgio.h"
#include "error.h"

StgInt
filePutc(StgAddr fp, I_ c)
{
    int rc;

    /* Try to write a character */
    while ((rc = putc((int) c, (FILE *) fp)) == EOF && errno == EINTR)
	clearerr((FILE *) fp);

    if (rc == EOF) {
	cvtErrno();
	stdErrno();
	return -1;
    }

    return 0;
}
