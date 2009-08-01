/* -----------------------------------------------------------------------------
 *
 * (c) Lennart Augustsson
 * (c) The GHC Team, 1998-2000
 *
 * Miscellaneous support for floating-point primitives
 *
 * ---------------------------------------------------------------------------*/

#include "HsFFI.h"
#include "Rts.h" // XXX wrong (for IEEE_FLOATING_POINT and WORDS_BIGENDIAN)

#define IEEE_FLOATING_POINT 1

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
#if FLOAT_WORDS_BIGENDIAN
	unsigned int mantissa0:20;
	unsigned int exponent:11;
	unsigned int negative:1;
	unsigned int mantissa1:32;
#else
	unsigned int mantissa1:32;
	unsigned int mantissa0:20;
	unsigned int exponent:11;
	unsigned int negative:1;
#endif
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
#if FLOAT_WORDS_BIGENDIAN
	unsigned int mantissa0:19;
	unsigned int quiet_nan:1;
	unsigned int exponent:11;
	unsigned int negative:1;
	unsigned int mantissa1:32;
#else
	unsigned int mantissa1:32;
	unsigned int mantissa0:19;
	unsigned int quiet_nan:1;
	unsigned int exponent:11;
	unsigned int negative:1;
#endif
#endif
   } ieee_nan;
};

/*
 * Predicates for testing for extreme IEEE fp values.
 */ 

/* In case you don't suppport IEEE, you'll just get dummy defs.. */
#ifdef IEEE_FLOATING_POINT

HsInt
isDoubleNaN(HsDouble d)
{
  union stg_ieee754_dbl u;
  
  u.d = d;

  return (
    u.ieee.exponent  == 2047 /* 2^11 - 1 */ &&  /* Is the exponent all ones? */
    (u.ieee.mantissa0 != 0 || u.ieee.mantissa1 != 0)
    	/* and the mantissa non-zero? */
    );
}

HsInt
isDoubleInfinite(HsDouble d)
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

HsInt
isDoubleDenormalized(HsDouble d) 
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

HsInt
isDoubleNegativeZero(HsDouble d) 
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

/* Same tests, this time for HsFloats. */

/*
 To recap, here's the representation of a single precision
 IEEE floating point number:

 sign         31           sign bit (0 == positive, 1 == negative)
 exponent     30-23        exponent (biased by 127)
 fraction     22-0         fraction (bits to right of binary point)
*/


HsInt
isFloatNaN(HsFloat f)
{
    union stg_ieee754_flt u;
    u.f = f;

   /* Floating point NaN iff exponent is all ones, mantissa is
      non-zero (but see below.) */
   return (
   	u.ieee.exponent == 255 /* 2^8 - 1 */ &&
	u.ieee.mantissa != 0);
}

HsInt
isFloatInfinite(HsFloat f)
{
    union stg_ieee754_flt u;
    u.f = f;
  
    /* A float is Inf iff exponent is max (all ones),
       and mantissa is min(all zeros.) */
    return (
    	u.ieee.exponent == 255 /* 2^8 - 1 */ &&
	u.ieee.mantissa == 0);
}

HsInt
isFloatDenormalized(HsFloat f)
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

HsInt
isFloatNegativeZero(HsFloat f) 
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
HsInt isDoubleNaN(d) HsDouble d; { return 0; }
HsInt isDoubleInfinite(d) HsDouble d; { return 0; }
HsInt isDoubleDenormalized(d) HsDouble d; { return 0; }
HsInt isDoubleNegativeZero(d) HsDouble d; { return 0; }
HsInt isFloatNaN(f) HsFloat f; { return 0; }
HsInt isFloatInfinite(f) HsFloat f; { return 0; }
HsInt isFloatDenormalized(f) HsFloat f; { return 0; }
HsInt isFloatNegativeZero(f) HsFloat f; { return 0; }

#endif /* ! IEEE_FLOATING_POINT */
