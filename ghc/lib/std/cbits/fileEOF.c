/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: fileEOF.c,v 1.3 1998/12/02 13:27:22 simonm Exp $
 *
 * hIsEOF Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

StgInt
fileEOF(ptr)
StgForeignPtr ptr;
{
    IOFileObject* fo = (IOFileObject*)ptr;

    if ( FILEOBJ_IS_EOF(fo) )
       return 1;

    if (fileLookAhead(ptr) != EOF)
	return 0;
    else if (ghc_errtype == ERR_EOF)
	return 1;
    else
	return -1;
}
