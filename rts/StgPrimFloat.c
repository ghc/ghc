/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2000
 *
 * Miscellaneous support for floating-point primitives
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include <math.h>

/*
 * Encoding and decoding Doubles.  Code based on the HBC code
 * (lib/fltcode.c).
 */

#ifdef _SHORT_LIMB
#define SIZEOF_LIMB_T SIZEOF_UNSIGNED_INT
#else
#ifdef _LONG_LONG_LIMB
#define SIZEOF_LIMB_T SIZEOF_UNSIGNED_LONG_LONG
#else
#define SIZEOF_LIMB_T SIZEOF_UNSIGNED_LONG
#endif
#endif

#if SIZEOF_LIMB_T == 4
#define GMP_BASE 4294967296.0
#elif SIZEOF_LIMB_T == 8
#define GMP_BASE 18446744073709551616.0
#else
#error Cannot cope with SIZEOF_LIMB_T -- please add definition of GMP_BASE
#endif

#define DNBIGIT	 ((SIZEOF_DOUBLE+SIZEOF_LIMB_T-1)/SIZEOF_LIMB_T)
#define FNBIGIT	 ((SIZEOF_FLOAT +SIZEOF_LIMB_T-1)/SIZEOF_LIMB_T)

#if IEEE_FLOATING_POINT
#define MY_DMINEXP  ((DBL_MIN_EXP) - (DBL_MANT_DIG) - 1)
/* DMINEXP is defined in values.h on Linux (for example) */
#define DHIGHBIT 0x00100000
#define DMSBIT   0x80000000

#define MY_FMINEXP  ((FLT_MIN_EXP) - (FLT_MANT_DIG) - 1)
#define FHIGHBIT 0x00800000
#define FMSBIT   0x80000000
#endif

#ifdef WORDS_BIGENDIAN
#define L 1
#define H 0
#else
#define L 0
#define H 1
#endif

#define __abs(a)		(( (a) >= 0 ) ? (a) : (-(a)))

StgDouble
__encodeDouble (I_ size, StgByteArray ba, I_ e) /* result = s * 2^e */
{
    StgDouble r;
    const mp_limb_t *const arr = (const mp_limb_t *)ba;
    I_ i;

    /* Convert MP_INT to a double; knows a lot about internal rep! */
    for(r = 0.0, i = __abs(size)-1; i >= 0; i--)
	r = (r * GMP_BASE) + arr[i];

    /* Now raise to the exponent */
    if ( r != 0.0 ) /* Lennart suggests this avoids a bug in MIPS's ldexp */
	r = ldexp(r, e);

    /* sign is encoded in the size */
    if (size < 0)
	r = -r;

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
    r = ldexp(r, e);
  
  /* sign is encoded in the size */
  if (j < 0)
    r = -r;
  
  return r;
}

StgFloat
__encodeFloat (I_ size, StgByteArray ba, I_ e) /* result = s * 2^e */
{
    StgFloat r;
    const mp_limb_t *arr = (const mp_limb_t *)ba;
    I_ i;

    /* Convert MP_INT to a float; knows a lot about internal rep! */
    for(r = 0.0, i = __abs(size)-1; i >= 0; i--)
	r = (r * GMP_BASE) + arr[i];

    /* Now raise to the exponent */
    if ( r != 0.0 ) /* Lennart suggests this avoids a bug in MIPS's ldexp */
	r = ldexp(r, e);

    /* sign is encoded in the size */
    if (size < 0)
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
    r = ldexp(r, e);
  
  /* sign is encoded in the size */
  if (j < 0)
    r = -r;
  
  return r;
}

/* This only supports IEEE floating point */

void
__decodeDouble (MP_INT *man, I_ *exp, StgDouble dbl)
{
    /* Do some bit fiddling on IEEE */
    unsigned int low, high; 	     	/* assuming 32 bit ints */
    int sign, iexp;
    union { double d; unsigned int i[2]; } u;	/* assuming 32 bit ints, 64 bit double */

    ASSERT(sizeof(unsigned int ) == 4            );
    ASSERT(sizeof(dbl          ) == SIZEOF_DOUBLE);
    ASSERT(sizeof(man->_mp_d[0]) == SIZEOF_LIMB_T);
    ASSERT(DNBIGIT*SIZEOF_LIMB_T >= SIZEOF_DOUBLE);

    u.d = dbl;	    /* grab chunks of the double */
    low = u.i[L];
    high = u.i[H];

    /* we know the MP_INT* passed in has size zero, so we realloc
    	no matter what.
    */
    man->_mp_alloc = DNBIGIT;

    if (low == 0 && (high & ~DMSBIT) == 0) {
	man->_mp_size = 0;
	*exp = 0L;
    } else {
	man->_mp_size = DNBIGIT;
	iexp = ((high >> 20) & 0x7ff) + MY_DMINEXP;
	sign = high;

	high &= DHIGHBIT-1;
	if (iexp != MY_DMINEXP)	/* don't add hidden bit to denorms */
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
#if DNBIGIT == 2
	man->_mp_d[0] = (mp_limb_t)low;
	man->_mp_d[1] = (mp_limb_t)high;
#else
#if DNBIGIT == 1
	man->_mp_d[0] = ((mp_limb_t)high) << 32 | (mp_limb_t)low;
#else
#error Cannot cope with DNBIGIT
#endif
#endif
	if (sign < 0)
	    man->_mp_size = -man->_mp_size;
    }
}

void
__decodeFloat (MP_INT *man, I_ *exp, StgFloat flt)
{
    /* Do some bit fiddling on IEEE */
    int high, sign; 	    	    /* assuming 32 bit ints */
    union { float f; int i; } u;    /* assuming 32 bit float and int */

    ASSERT(sizeof(int          ) == 4            );
    ASSERT(sizeof(flt          ) == SIZEOF_FLOAT );
    ASSERT(sizeof(man->_mp_d[0]) == SIZEOF_LIMB_T);
    ASSERT(FNBIGIT*SIZEOF_LIMB_T >= SIZEOF_FLOAT );

    u.f = flt;	    /* grab the float */
    high = u.i;

    /* we know the MP_INT* passed in has size zero, so we realloc
    	no matter what.
    */
    man->_mp_alloc = FNBIGIT;

    if ((high & ~FMSBIT) == 0) {
	man->_mp_size = 0;
	*exp = 0;
    } else {
	man->_mp_size = FNBIGIT;
	*exp = ((high >> 23) & 0xff) + MY_FMINEXP;
	sign = high;

	high &= FHIGHBIT-1;
	if (*exp != MY_FMINEXP)	/* don't add hidden bit to denorms */
	    high |= FHIGHBIT;
	else {
	    (*exp)++;
	    /* A denorm, normalize the mantissa */
	    while (! (high & FHIGHBIT)) {
		high <<= 1;
		(*exp)--;
	    }
	}
#if FNBIGIT == 1
	man->_mp_d[0] = (mp_limb_t)high;
#else
#error Cannot cope with FNBIGIT
#endif
	if (sign < 0)
	    man->_mp_size = -man->_mp_size;
    }
}

/* Convenient union types for checking the layout of IEEE 754 types -
   based on defs in GNU libc <ieee754.h>
*/

union stg_ieee754_flt
{
   float f;
   struct {

#if WORDS_BIGENDIAN
	unsigned int negative:1;
	unsigned int exponent:8;
	unsigned int mantissa:23;
#else
	unsigned int mantissa:23;
	unsigned int exponent:8;
	unsigned int negative:1;
#endif
   } ieee;
   struct {

#if WORDS_BIGENDIAN
	unsigned int negative:1;
	unsigned int exponent:8;
	unsigned int quiet_nan:1;
	unsigned int mantissa:22;
#else
	unsigned int mantissa:22;
	unsigned int quiet_nan:1;
	unsigned int exponent:8;
	unsigned int negative:1;
#endif
   } ieee_nan;
};

/*
 
 To recap, here's the representation of a double precision
 IEEE floating point number:

 sign         63           sign bit (0==positive, 1==negative)
 exponent     62-52        exponent (biased by 1023)
 fraction     51-0         fraction (bits to right of binary point)
*/

union stg_ieee754_dbl
{
   double d;
   struct {

#if WORDS_BIGENDIAN
	unsigned int negative:1;
	unsigned int exponent:11;
	unsigned int mantissa0:20;
	unsigned int mantissa1:32;
#else
	unsigned int mantissa1:32;
	unsigned int mantissa0:20;
	unsigned int exponent:11;
	unsigned int negative:1;
#endif
   } ieee;
    /* This format makes it easier to see if a NaN is a signalling NaN.  */
   struct {

#if WORDS_BIGENDIAN
	unsigned int negative:1;
	unsigned int exponent:11;
	unsigned int quiet_nan:1;
	unsigned int mantissa0:19;
	unsigned int mantissa1:32;
#else
	unsigned int mantissa1:32;
	unsigned int mantissa0:19;
	unsigned int quiet_nan:1;
	unsigned int exponent:11;
	unsigned int negative:1;
#endif
   } ieee_nan;
};

/*
 * Predicates for testing for extreme IEEE fp values. Used
 * by the bytecode evaluator and the Prelude.
 *
 */ 

/* In case you don't suppport IEEE, you'll just get dummy defs.. */
#ifdef IEEE_FLOATING_POINT

StgInt
isDoubleNaN(StgDouble d)
{
  union stg_ieee754_dbl u;
  
  u.d = d;

  return (
    u.ieee.exponent  == 2047 /* 2^11 - 1 */ &&  /* Is the exponent all ones? */
    (u.ieee.mantissa0 != 0 || u.ieee.mantissa1 != 0)
    	/* and the mantissa non-zero? */
    );
}

StgInt
isDoubleInfinite(StgDouble d)
{
    union stg_ieee754_dbl u;

    u.d = d;

    /* Inf iff exponent is all ones, mantissa all zeros */
    return (
        u.ieee.exponent  == 2047 /* 2^11 - 1 */ &&
	u.ieee.mantissa0 == 0 		        &&
	u.ieee.mantissa1 == 0
      );
}

StgInt
isDoubleDenormalized(StgDouble d) 
{
    union stg_ieee754_dbl u;

    u.d = d;

    /* A (single/double/quad) precision floating point number
       is denormalised iff:
        - exponent is zero
	- mantissa is non-zero.
        - (don't care about setting of sign bit.)

    */
    return (  
	u.ieee.exponent  == 0 &&
	(u.ieee.mantissa0 != 0 ||
	 u.ieee.mantissa1 != 0)
      );
	 
}

StgInt
isDoubleNegativeZero(StgDouble d) 
{
    union stg_ieee754_dbl u;

    u.d = d;
    /* sign (bit 63) set (only) => negative zero */

    return (
    	u.ieee.negative  == 1 &&
	u.ieee.exponent  == 0 &&
	u.ieee.mantissa0 == 0 &&
	u.ieee.mantissa1 == 0);
}

/* Same tests, this time for StgFloats. */

/*
 To recap, here's the representation of a single precision
 IEEE floating point number:

 sign         31           sign bit (0 == positive, 1 == negative)
 exponent     30-23        exponent (biased by 127)
 fraction     22-0         fraction (bits to right of binary point)
*/


StgInt
isFloatNaN(StgFloat f)
{
    union stg_ieee754_flt u;
    u.f = f;

   /* Floating point NaN iff exponent is all ones, mantissa is
      non-zero (but see below.) */
   return (
   	u.ieee.exponent == 255 /* 2^8 - 1 */ &&
	u.ieee.mantissa != 0);
}

StgInt
isFloatInfinite(StgFloat f)
{
    union stg_ieee754_flt u;
    u.f = f;
  
    /* A float is Inf iff exponent is max (all ones),
       and mantissa is min(all zeros.) */
    return (
    	u.ieee.exponent == 255 /* 2^8 - 1 */ &&
	u.ieee.mantissa == 0);
}

StgInt
isFloatDenormalized(StgFloat f)
{
    union stg_ieee754_flt u;
    u.f = f;

    /* A (single/double/quad) precision floating point number
       is denormalised iff:
        - exponent is zero
	- mantissa is non-zero.
        - (don't care about setting of sign bit.)

    */
    return (
    	u.ieee.exponent == 0 &&
	u.ieee.mantissa != 0);
}

StgInt
isFloatNegativeZero(StgFloat f) 
{
    union stg_ieee754_flt u;
    u.f = f;

    /* sign (bit 31) set (only) => negative zero */
    return (
	u.ieee.negative      &&
	u.ieee.exponent == 0 &&
	u.ieee.mantissa == 0);
}

#else /* ! IEEE_FLOATING_POINT */

/* Dummy definitions of predicates - they all return false */
StgInt isDoubleNaN(d) StgDouble d; { return 0; }
StgInt isDoubleInfinite(d) StgDouble d; { return 0; }
StgInt isDoubleDenormalized(d) StgDouble d; { return 0; }
StgInt isDoubleNegativeZero(d) StgDouble d; { return 0; }
StgInt isFloatNaN(f) StgFloat f; { return 0; }
StgInt isFloatInfinite(f) StgFloat f; { return 0; }
StgInt isFloatDenormalized(f) StgFloat f; { return 0; }
StgInt isFloatNegativeZero(f) StgFloat f; { return 0; }

#endif /* ! IEEE_FLOATING_POINT */
