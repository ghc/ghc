/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: readFile.c,v 1.1 1998/04/10 10:54:43 simonm Exp $
 *
 * hGetContents Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#define EOT 4

StgInt
readBlock(StgAddr buf, StgAddr fp, StgInt size)
{
    int count;

    if (feof((FILE *) fp)) {
	ghc_errtype = ERR_EOF;
	ghc_errstr = "";
	return -1;
    }

    while ((count = fread(buf, 1, size, (FILE *) fp)) == 0) {
	if (feof((FILE *) fp)) {
	    ghc_errtype = ERR_EOF;
	    ghc_errstr = "";
	    return -1;
	} else if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
	clearerr((FILE *) fp);
    }

    return count;
}

StgInt
readLine(StgAddr buf, StgAddr fp, StgInt size)
{
    if (feof((FILE *) fp)) {
	ghc_errtype = ERR_EOF;
	ghc_errstr = "";
	return -1;
    }

    while (fgets(buf, size, (FILE *) fp) == NULL) {
	if (feof((FILE *) fp)) {
	    ghc_errtype = ERR_EOF;
	    ghc_errstr = "";
	    return -1;
	} else if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
	clearerr((FILE *) fp);
    }

    return strlen(buf);
}

StgInt
readChar(StgAddr fp)
{
    int c;

    if (feof((FILE *) fp)) {
	ghc_errtype = ERR_EOF;
	ghc_errstr = "";
	return -1;
    }

    while ((c = getc((FILE *) fp)) == EOF) {
	if (feof((FILE *) fp)) {
	    ghc_errtype = ERR_EOF;
	    ghc_errstr = "";
	    return -1;
	} else if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
	clearerr((FILE *) fp);
    }

    if (isatty(fileno((FILE *) fp)) && c == EOT) 
	return EOF;
    else
        return c;
}
