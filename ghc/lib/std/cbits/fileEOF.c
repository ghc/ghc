/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: fileEOF.c,v 1.4 1999/11/25 16:54:14 simonmar Exp $
 *
 * hIsEOF Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

StgInt
fileEOF(StgForeignPtr ptr)
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
