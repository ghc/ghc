/* -----------------------------------------------------------------------------
 *
 * (c) Lennart Augustsson
 * (c) The GHC Team, 1998-2000
 *
 * Miscellaneous support for floating-point primitives
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "StgPrimFloat.h"

#include <math.h>
#include <float.h>
#include <limits.h>

#define IEEE_FLOATING_POINT 1

#if FLT_RADIX != 2
# error FLT_RADIX != 2 not supported
#endif

/*
 * Encoding and decoding Doubles.  Code based on the HBC code
 * (lib/fltcode.c).
 */

#if IEEE_FLOATING_POINT
#define MY_DMINEXP  ((DBL_MIN_EXP) - (DBL_MANT_DIG) - 1)
/* DMINEXP is defined in values.h on Linux (for example) */
#define DHIGHBIT 0x00100000
#define DMSBIT   0x80000000

#define MY_FMINEXP  ((FLT_MIN_EXP) - (FLT_MANT_DIG) - 1)
#define FHIGHBIT 0x00800000
#define FMSBIT   0x80000000
#endif

#if defined(WORDS_BIGENDIAN) || defined(FLOAT_WORDS_BIGENDIAN)
#define L 1
#define H 0
#else
#define L 0
#define H 1
#endif

#define __abs(a)                (( (a) >= 0 ) ? (a) : (-(a)))

/** Trac #15271: Some large ratios are converted into double incorrectly.
  * This occurs when StgInt has 64 bits and C int has 32 bits, where wrapping
  * occurs and an incorrect signed value is passed into ldexp */
STATIC_INLINE int
truncExponent(I_ e)
{
#if INT_MAX < STG_INT_MAX
  if (RTS_UNLIKELY(e > INT_MAX))
    e = INT_MAX;
#endif
#if INT_MIN > STG_INT_MIN
  if (RTS_UNLIKELY(e < INT_MIN))
    e = INT_MIN;
#endif
  return e;
}

/* Special version for words */
StgDouble
__word_encodeDouble (W_ j, I_ e)
{
  StgDouble r;

  r = (StgDouble)j;

  /* Now raise to the exponent */
  if ( r != 0.0 ) /* Lennart suggests this avoids a bug in MIPS's ldexp */
    r = ldexp(r, truncExponent(e));

  return r;
}

/* Special version for small Integers */
StgDouble
__int_encodeDouble (I_ j, I_ e)
{
  StgDouble r;

  r = (StgDouble)__abs(j);

  /* Now raise to the exponent */
  if ( r != 0.0 ) /* Lennart suggests this avoids a bug in MIPS's ldexp */
    r = ldexp(r, truncExponent(e));

  /* sign is encoded in the size */
  if (j < 0)
    r = -r;

  return r;
}

/* Special version for small Integers */
StgFloat
__int_encodeFloat (I_ j, I_ e)
{
  StgFloat r;

  r = (StgFloat)__abs(j);

  /* Now raise to the exponent */
  if ( r != 0.0 ) /* Lennart suggests this avoids a bug in MIPS's ldexp */
    r = ldexp(r, truncExponent(e));

  /* sign is encoded in the size */
  if (j < 0)
    r = -r;

  return r;
}

/* Special version for small positive Integers */
StgFloat
__word_encodeFloat (W_ j, I_ e)
{
  StgFloat r;

  r = (StgFloat)j;

  /* Now raise to the exponent */
  if ( r != 0.0 ) /* Lennart suggests this avoids a bug in MIPS's ldexp */
    r = ldexp(r, truncExponent(e));

  return r;
}

/* This only supports IEEE floating point */

void
__decodeDouble_2Int (I_ *man_sign, W_ *man_high, W_ *man_low, I_ *exp, StgDouble dbl)
{
    /* Do some bit fiddling on IEEE */
    unsigned int low, high;             /* assuming 32 bit ints */
    int sign, iexp;
    union { double d; unsigned int i[2]; } u;   /* assuming 32 bit ints, 64 bit double */

    ASSERT(sizeof(unsigned int ) == 4            );
    ASSERT(sizeof(dbl          ) == 8            );
    ASSERT(sizeof(dbl          ) == SIZEOF_DOUBLE);

    u.d = dbl;      /* grab chunks of the double */
    low = u.i[L];
    high = u.i[H];

    if (low == 0 && (high & ~DMSBIT) == 0) {
        *man_low = 0;
        *man_high = 0;
        *exp = 0L;
    } else {
        iexp = ((high >> 20) & 0x7ff) + MY_DMINEXP;
        sign = high;

        high &= DHIGHBIT-1;
        if (iexp != MY_DMINEXP) /* don't add hidden bit to denorms */
            high |= DHIGHBIT;
        else {
            iexp++;
            /* A denorm, normalize the mantissa */
            while (! (high & DHIGHBIT)) {
                high <<= 1;
                if (low & DMSBIT)
                    high++;
                low <<= 1;
                iexp--;
            }
        }
        *exp = (I_) iexp;
        *man_low = low;
        *man_high = high;
        *man_sign = (sign < 0) ? -1 : 1;
    }
}

/* This is expected to replace uses of __decodeDouble_2Int() in the long run */
StgInt
__decodeDouble_Int64 (StgInt64 *const mantissa, const StgDouble dbl)
{
#if 0
    // We can't use this yet as-is, see ticket #9810
    if (dbl) {
        int exp = 0;
        *mantissa = (StgInt64)scalbn(frexp(dbl, &exp), DBL_MANT_DIG);
        return exp-DBL_MANT_DIG;
    } else {
        *mantissa = 0;
        return 0;
    }
#else
    I_ man_sign = 0;
    W_ man_high = 0, man_low = 0;
    I_ exp = 0;

    __decodeDouble_2Int (&man_sign, &man_high, &man_low, &exp, dbl);
    ASSIGN_Int64((W_*)mantissa, ((((StgInt64)man_high << 32)
                                  | (StgInt64)man_low)
                                 * (StgInt64)man_sign));
    return exp;
#endif
}

/* Convenient union types for checking the layout of IEEE 754 types -
   based on defs in GNU libc <ieee754.h>
*/

void
__decodeFloat_Int (I_ *man, I_ *exp, StgFloat flt)
{
    /* Do some bit fiddling on IEEE */
    int high, sign;                 /* assuming 32 bit ints */
    union { float f; int i; } u;    /* assuming 32 bit float and int */

    ASSERT(sizeof(int          ) == 4            );
    ASSERT(sizeof(flt          ) == 4            );
    ASSERT(sizeof(flt          ) == SIZEOF_FLOAT );

    u.f = flt;      /* grab the float */
    high = u.i;

    if ((high & ~FMSBIT) == 0) {
        *man = 0;
        *exp = 0;
    } else {
        *exp = ((high >> 23) & 0xff) + MY_FMINEXP;
        sign = high;

        high &= FHIGHBIT-1;
        if (*exp != MY_FMINEXP) /* don't add hidden bit to denorms */
            high |= FHIGHBIT;
        else {
            (*exp)++;
            /* A denorm, normalize the mantissa */
            while (! (high & FHIGHBIT)) {
                high <<= 1;
                (*exp)--;
            }
        }
        *man = high;
        if (sign < 0)
            *man = - *man;
    }
}

