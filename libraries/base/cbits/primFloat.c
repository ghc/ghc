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
isDoubleFinite(HsDouble d)
{
  union stg_ieee754_dbl u;

  u.d = d;

  return u.ieee.exponent != 2047;
}

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
isFloatFinite(HsFloat f)
{
    union stg_ieee754_flt u;
    u.f = f;
    return u.ieee.exponent != 255;
}

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

/*
 There are glibc versions around with buggy rintf or rint, hence we
 provide our own. We always round ties to even, so we can be simpler.
*/

#define FLT_HIDDEN 0x800000
#define FLT_POWER2 0x1000000

HsFloat
rintFloat(HsFloat f)
{
    union stg_ieee754_flt u;
    u.f = f;
    /* if real exponent > 22, it's already integral, infinite or nan */
    if (u.ieee.exponent > 149)  /* 22 + 127 */
    {
        return u.f;
    }
    if (u.ieee.exponent < 126)  /* (-1) + 127, abs(f) < 0.5 */
    {
        /* only used for rounding to Integral a, so don't care about -0.0 */
        return 0.0;
    }
    /* 0.5 <= abs(f) < 2^23 */
    unsigned int half, mask, mant, frac;
    half = 1 << (149 - u.ieee.exponent);    /* bit for 0.5 */
    mask = 2*half - 1;                      /* fraction bits */
    mant = u.ieee.mantissa | FLT_HIDDEN;    /* add hidden bit */
    frac = mant & mask;                     /* get fraction */
    mant ^= frac;                           /* truncate mantissa */
    if ((frac < half) || ((frac == half) && ((mant & (2*half)) == 0)))
    {
        /* this means we have to truncate */
        if (mant == 0)
        {
            /* f == ±0.5, return 0.0 */
            return 0.0;
        }
        else
        {
            /* remove hidden bit and set mantissa */
            u.ieee.mantissa = mant ^ FLT_HIDDEN;
            return u.f;
        }
    }
    else
    {
        /* round away from zero, increment mantissa */
        mant += 2*half;
        if (mant == FLT_POWER2)
        {
            /* next power of 2, increase exponent an set mantissa to 0 */
            u.ieee.mantissa = 0;
            u.ieee.exponent += 1;
            return u.f;
        }
        else
        {
            /* remove hidden bit and set mantissa */
            u.ieee.mantissa = mant ^ FLT_HIDDEN;
            return u.f;
        }
    }
}

#define DBL_HIDDEN 0x100000
#define DBL_POWER2 0x200000
#define LTOP_BIT 0x80000000

HsDouble
rintDouble(HsDouble d)
{
    union stg_ieee754_dbl u;
    u.d = d;
    /* if real exponent > 51, it's already integral, infinite or nan */
    if (u.ieee.exponent > 1074) /* 51 + 1023 */
    {
        return u.d;
    }
    if (u.ieee.exponent < 1022)  /* (-1) + 1023, abs(d) < 0.5 */
    {
        /* only used for rounding to Integral a, so don't care about -0.0 */
        return 0.0;
    }
    unsigned int half, mask, mant, frac;
    if (u.ieee.exponent < 1043) /* 20 + 1023, real exponent < 20 */
    {
        /* the fractional part meets the higher part of the mantissa */
        half = 1 << (1042 - u.ieee.exponent);   /* bit for 0.5 */
        mask = 2*half - 1;                      /* fraction bits */
        mant = u.ieee.mantissa0 | DBL_HIDDEN;   /* add hidden bit */
        frac = mant & mask;                     /* get fraction */
        mant ^= frac;                           /* truncate mantissa */
        if ((frac < half) ||
            ((frac == half) && (u.ieee.mantissa1 == 0)  /* a tie */
                && ((mant & (2*half)) == 0)))
        {
            /* truncate */
            if (mant == 0)
            {
                /* d = ±0.5, return 0.0 */
                return 0.0;
            }
            /* remove hidden bit and set mantissa */
            u.ieee.mantissa0 = mant ^ DBL_HIDDEN;
            u.ieee.mantissa1 = 0;
            return u.d;
        }
        else    /* round away from zero */
        {
            /* zero low mantissa bits */
            u.ieee.mantissa1 = 0;
            /* increment integer part of mantissa */
            mant += 2*half;
            if (mant == DBL_POWER2)
            {
                /* power of 2, increment exponent and zero mantissa */
                u.ieee.mantissa0 = 0;
                u.ieee.exponent += 1;
                return u.d;
            }
            /* remove hidden bit */
            u.ieee.mantissa0 = mant ^ DBL_HIDDEN;
            return u.d;
        }
    }
    else
    {
        /* 20 <= real exponent < 52, fractional part entirely in mantissa1 */
        half = 1 << (1074 - u.ieee.exponent);   /* bit for 0.5 */
        mask = 2*half - 1;                      /* fraction bits */
        mant = u.ieee.mantissa1;                /* no hidden bit here */
        frac = mant & mask;                     /* get fraction */
        mant ^= frac;                           /* truncate mantissa */
        if ((frac < half) ||
            ((frac == half) &&                  /* tie */
            (((half == LTOP_BIT) ? (u.ieee.mantissa0 & 1)  /* yuck */
                                : (mant & (2*half)))
                                        == 0)))
        {
            /* truncate */
            u.ieee.mantissa1 = mant;
            return u.d;
        }
        else
        {
            /* round away from zero */
            /* increment mantissa */
            mant += 2*half;
            u.ieee.mantissa1 = mant;
            if (mant == 0)
            {
                /* low part of mantissa overflowed */
                /* increment high part of mantissa */
                mant = u.ieee.mantissa0 + 1;
                if (mant == DBL_HIDDEN)
                {
                    /* hit power of 2 */
                    /* zero mantissa */
                    u.ieee.mantissa0 = 0;
                    /* and increment exponent */
                    u.ieee.exponent += 1;
                    return u.d;
                }
                else
                {
                    u.ieee.mantissa0 = mant;
                    return u.d;
                }
            }
            else
            {
                return u.d;
            }
        }
    }
}

#else /* ! IEEE_FLOATING_POINT */

/* Dummy definitions of predicates - they all return "normal" values */
HsInt isDoubleFinite(HsDouble d) { return 1;}
HsInt isDoubleNaN(HsDouble d) { return 0; }
HsInt isDoubleInfinite(HsDouble d) { return 0; }
HsInt isDoubleDenormalized(HsDouble d) { return 0; }
HsInt isDoubleNegativeZero(HsDouble d) { return 0; }
HsInt isFloatFinite(HsFloat f) { return 1; }
HsInt isFloatNaN(HsFloat f) { return 0; }
HsInt isFloatInfinite(HsFloat f) { return 0; }
HsInt isFloatDenormalized(HsFloat f) { return 0; }
HsInt isFloatNegativeZero(HsFloat f) { return 0; }


/* For exotic floating point formats, we can't do much */
/* We suppose the format has not too many bits */
/* I hope nobody tries to build GHC where this is wrong */

#define FLT_UPP 536870912.0

HsFloat
rintFloat(HsFloat f)
{
    if ((f > FLT_UPP) || (f < (-FLT_UPP)))
    {
        return f;
    }
    else
    {
        int i = (int)f;
        float g = i;
        float d = f - g;
        if (d > 0.5)
        {
            return g + 1.0;
        }
        if (d == 0.5)
        {
            return (i & 1) ? (g + 1.0) : g;
        }
        if (d == -0.5)
        {
            return (i & 1) ? (g - 1.0) : g;
        }
        if (d < -0.5)
        {
            return g - 1.0;
        }
        return g;
    }
}

#define DBL_UPP 2305843009213693952.0

HsDouble
rintDouble(HsDouble d)
{
    if ((d > DBL_UPP) || (d < (-DBL_UPP)))
    {
        return d;
    }
    else
    {
        HsInt64 i = (HsInt64)d;
        double e = i;
        double r = d - e;
        if (r > 0.5)
        {
            return e + 1.0;
        }
        if (r == 0.5)
        {
            return (i & 1) ? (e + 1.0) : e;
        }
        if (r == -0.5)
        {
            return (i & 1) ? (e - 1.0) : e;
        }
        if (r < -0.5)
        {
            return e - 1.0;
        }
        return e;
    }
}

#endif /* ! IEEE_FLOATING_POINT */
