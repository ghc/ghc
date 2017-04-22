/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Primitive floating-point operations
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

/* grimy low-level support functions defined in StgPrimFloat.c */
StgInt    __decodeDouble_Int64 (StgInt64 *mantissa, StgDouble dbl);
void      __decodeDouble_2Int (I_ *man_sign, W_ *man_high, W_ *man_low, I_ *exp, StgDouble dbl);
void      __decodeFloat_Int (I_ *man, I_ *exp, StgFloat flt);

// __{int,word}_encode{Float,Double} are public, declared in
// includes/rts/PrimFloat.h.

#include "EndPrivate.h"
