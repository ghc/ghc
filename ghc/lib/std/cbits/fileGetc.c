/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: fileGetc.c,v 1.1 1998/04/10 10:54:22 simonm Exp $
 *
 * hGetChar Runtime Support
 */

#include "Rts.h"
#include "stgio.h"
#include "error.h"

StgInt
fileGetc(StgAddr fp)
{
    int c;

    if (feof((FILE *)fp)) {
	ghc_errtype = ERR_EOF;
	ghc_errstr = "";
	return EOF;
    }

    /* Try to read a character */
    while ((c = getc((FILE *)fp)) == EOF && errno == EINTR)
	clearerr((FILE *)fp);

    if (feof((FILE *)fp)) {
	ghc_errtype = ERR_EOF;
	ghc_errstr = "";
    } else if (c == EOF) {
	cvtErrno();
	stdErrno();
    }
    return c;
}
