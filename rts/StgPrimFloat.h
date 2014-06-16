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

// __{int,word}_encode{Float,Double} are public, declared in 
// includes/rts/PrimFloat.h.

#include "EndPrivate.h"

#endif /* STGPRIMFLOAT_H */
