/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: fileLookAhead.c,v 1.1 1998/04/10 10:54:24 simonm Exp $
 *
 * hLookAhead Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

StgInt
fileLookAhead(StgAddr fp)
{
    int c;

    if ((c = fileGetc((FILE *)fp)) == EOF) {
	return c;
    } else if (ungetc(c, (FILE *)fp) == EOF) {
	cvtErrno();
	stdErrno();
	return EOF;
    } else
	return c;
}
