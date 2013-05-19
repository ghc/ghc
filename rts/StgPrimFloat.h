/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Primitive floating-point operations
 *
 * ---------------------------------------------------------------------------*/

#ifndef STGPRIMFLOAT_H
#define STGPRIMFLOAT_H

#include "BeginPrivate.h"

/* grimy low-level support functions defined in StgPrimFloat.c */
void      __decodeDouble_2Int (I_ *man_sign, W_ *man_high, W_ *man_low, I_ *exp, StgDouble dbl);
void      __decodeFloat_Int (I_ *man, I_ *exp, StgFloat flt);
StgDouble __word_encodeDouble (W_ j, I_ e);
StgFloat  __word_encodeFloat (W_ j, I_ e);

// __int_encodeDouble and __int_encodeFloat are public, declared in 
// includes/rts/PrimFloat.h.

#include "EndPrivate.h"

#endif /* STGPRIMFLOAT_H */
