/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: fileEOF.c,v 1.1 1998/04/10 10:54:21 simonm Exp $
 *
 * hIsEOF Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

StgInt
fileEOF(StgAddr fp)
{
    if (fileLookAhead(fp) != EOF)
	return 0;
    else if (ghc_errtype == ERR_EOF)
	return 1;
    else
	return -1;
}
