/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Primitive floating-point operations
 *
 * ---------------------------------------------------------------------------*/

#ifndef STGPRIMFLOAT_H
#define STGPRIMFLOAT_H

/* grimy low-level support functions defined in StgPrimFloat.c */
extern void      __decodeDouble_2Int (I_ *man_sign, W_ *man_high, W_ *man_low, I_ *exp, StgDouble dbl);
extern void      __decodeFloat_Int (I_ *man, I_ *exp, StgFloat flt);
extern StgDouble __2Int_encodeDouble (I_ j_high, I_ j_low, I_ e);
extern StgDouble __int_encodeDouble (I_ j, I_ e);
extern StgDouble __word_encodeDouble (W_ j, I_ e);
extern StgFloat  __int_encodeFloat (I_ j, I_ e);
extern StgFloat  __word_encodeFloat (W_ j, I_ e);

#endif /* STGPRIMFLOAT_H */
