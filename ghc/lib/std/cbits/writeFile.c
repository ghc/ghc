/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: writeFile.c,v 1.1 1998/04/10 10:55:00 simonm Exp $
 *
 * hPutStr Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

StgInt
writeFile(StgAddr buf, StgAddr fp, StgInt bytes)
{
    int count;
    char *p = (char *) buf;

    if (bytes == 0)
	return 0;

    /* Disallow short writes */
    while ((count = fwrite(p, 1, bytes, (FILE *) fp)) < bytes) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
	bytes -= count;
	p += count;
	clearerr((FILE *) fp);
    }

    return 0;
}


StgInt
writeBuf(StgAddr fp, W_ elt_sz, I_ len, StgAddr buf)
{
    int count;
    char *p = (char *) buf;

    if (len == 0 || elt_sz == 0)
	return 0;

    /* Disallow short writes */
    while ((count = fwrite((char *)buf, (unsigned)elt_sz, (int)len, (FILE *) fp)) < len) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
	len -= count;
	p += count;
	clearerr((FILE *) fp);
    }

    return 0;
}
