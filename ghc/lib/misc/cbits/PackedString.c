/* -----------------------------------------------------------------------------
 * $Id: PackedString.c,v 1.2 1998/12/02 13:26:41 simonm Exp $
 *
 * PackedString C bits
 *
 * (c) The GHC Team 1998
 * -------------------------------------------------------------------------- */

#include "Rts.h"

StgInt
byteArrayHasNUL__ (StgByteArray ba, StgInt len)
{
    StgInt i;

    for (i = 0; i < len; i++) {
	if (*(ba + i) == '\0') {
	    return(1); /* true */
	}
    }

    return(0); /* false */
}
