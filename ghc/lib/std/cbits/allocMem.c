/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: allocMem.c,v 1.3 1999/11/25 16:54:14 simonmar Exp $
 *
 * malloc interface
 */

#include "Rts.h"
#include "stgio.h"

StgAddr
allocMemory__(StgInt sz/* bytes */)
{
 StgAddr ptr;

 if ( (ptr = malloc(sz*sizeof(char))) == NULL) {
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr  = "malloc failed";
	return NULL;
 }
 return ptr;

}
