/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: allocMem.c,v 1.2 1998/12/02 13:27:13 simonm Exp $
 *
 * malloc interface
 */

#include "Rts.h"
#include "stgio.h"

StgAddr
allocMemory__(sz)
StgInt sz;/* bytes*/
{
 StgAddr ptr;

 if ( (ptr = malloc(sz*sizeof(char))) == NULL) {
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr  = "malloc failed";
	return NULL;
 }
 return ptr;

}
