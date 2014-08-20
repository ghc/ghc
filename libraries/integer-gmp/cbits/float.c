/* -----------------------------------------------------------------------------
 *
 * (c) Lennart Augustsson
 * (c) The GHC Team, 1998-2000
 *
 * Support for floating-point <-> gmp integer primitives
 *
 * ---------------------------------------------------------------------------*/

/* TODO: do we need PosixSource.h ? it lives in rts/ not public includes/ */
/* #include "PosixSource.h" */
#include "Rts.h"
#include "gmp.h"
#include "GmpDerivedConstants.h"

#include <math.h>

#define IEEE_FLOATING_POINT 1

/*
 * Encoding and decoding Doubles.  Code based on the HBC code
 * (lib/fltcode.c).
 */

#define SIZEOF_LIMB_T SIZEOF_MP_LIMB_T

#if SIZEOF_LIMB_T == 4
#define GMP_BASE 4294967296.0
#define LIMBBITS_LOG_2 5
#elif SIZEOF_LIMB_T == 8
#define GMP_BASE 18446744073709551616.0
#define LIMBBITS_LOG_2 6
#else
#error Cannot cope with SIZEOF_LIMB_T -- please add definition of GMP_BASE
#endif

#define DNBIGIT  ((SIZEOF_DOUBLE+SIZEOF_LIMB_T-1)/SIZEOF_LIMB_T)
#define FNBIGIT  ((SIZEOF_FLOAT +SIZEOF_LIMB_T-1)/SIZEOF_LIMB_T)

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

StgDouble
integer_cbits_encodeDouble (I_ size, StgByteArray ba, I_ e) /* result = s * 2^e */
{
    StgDouble r;
    const mp_limb_t *const arr = (const mp_limb_t *)ba;
    I_ i;

    /* Convert MP_INT to a double; knows a lot about internal rep! */
    i = __abs(size)-1;
    if ((i < 15) || (e >= 0)) /* overflows only if the final result does */
    {
        /* This would cause overflow if a large MP_INT is passed, even if the
         * exponent would scale it back into range, so we do it only when it's safe. */
        for(r = 0.0; i >= 0; i--)
            r = (r * GMP_BASE) + arr[i];

    } else { /* possibly more than 1024 bits in the MP_INT, but gets scaled down */

        /* Find the first nonzero limb; normally it would be the first */
        r = 0.0;
        while((i >= 0) && (r == 0.0))
        {
            r = arr[i--];
        }
        if (i >= 0)
            r = (r * GMP_BASE) + arr[i];
#if SIZEOF_LIMB_T < 8
        if (i > 0)
            r = (r * GMP_BASE) + arr[--i];
#endif
        /* Now we have at least the 65 leading bits of the MP_INT or all of it.
         * Any further bits would be rounded down, so from now on everything is
         * multiplication by powers of 2.
         * If i is positive, arr contains i limbs we haven't looked at yet, so
         * adjust the exponent by i*8*SIZEOF_LIMB_T. Unfortunately, we must
         * beware of overflow, so we can't simply add this to e. */
        if (i > 0)
        {
            /* first add the number of whole limbs that would be cancelled */
            i = i + e / (8 * SIZEOF_LIMB_T);
            /* check for overflow */
            if ((i > 0) && ((i >> (8*sizeof(I_) - 1 - LIMBBITS_LOG_2)) > 0))
            {
                /* overflow, give e a large dummy value */
                e = 2147483647;
            } else {
                /* no overflow, get the exact value */
                e = i * (8 * SIZEOF_LIMB_T) + (e % (8 * SIZEOF_LIMB_T));
            }
        }
    }

    /* Now raise to the exponent */
    if ( r != 0.0 ) /* Lennart suggests this avoids a bug in MIPS's ldexp */
        r = ldexp(r, e);

    /* sign is encoded in the size */
    if (size < 0)
        r = -r;

    return r;
}

StgFloat
integer_cbits_encodeFloat (I_ size, StgByteArray ba, I_ e) /* result = s * 2^e */
{
    StgFloat r;
    const mp_limb_t *arr = (const mp_limb_t *)ba;
    I_ i;

    /* Convert MP_INT to a float; knows a lot about internal rep! */
    i = __abs(size)-1;
    /* just in case StgFloat is a double, check sizes */
#if SIZEOF_FLOAT == 4
    if ((i < 2) || (e >= 0))
#else
    if ((i < 15) || (e >= 0))
#endif
    {
        for(r = 0.0; i >= 0; i--)
            r = (r * GMP_BASE) + arr[i];
    } else {

        /* Find the first nonzero limb; normally it would be the first */
        r = 0.0;
        while((i >= 0) && (r == 0.0))
        {
            r = arr[i--];
        }
        if (i >= 0)
            r = (r * GMP_BASE) + arr[i];
#if (SIZEOF_LIMB_T < 8) && (SIZEOF_FLOAT > 4)
        if (i > 0)
            r = (r * GMP_BASE) + arr[--i];
#endif
        /* Now we have enough leading bits of the MP_INT.
         * Any further bits would be rounded down, so from now on everything is
         * multiplication by powers of 2.
         * If i is positive, arr contains i limbs we haven't looked at yet, so
         * adjust the exponent by i*8*SIZEOF_LIMB_T. Unfortunately, we must
         * beware of overflow, so we can't simply add this to e. */
        if (i > 0)
        {
            /* first add the number of whole limbs that would be cancelled */
            i = i + e / (8 * SIZEOF_LIMB_T);
            /* check for overflow */
            if ((i > 0) && ((i >> (8*sizeof(I_) - 1 - LIMBBITS_LOG_2)) > 0))
            {
                /* overflow, give e a large dummy value */
                e = 2147483647;
            } else {
                /* no overflow, get the exact value */
                e = i * (8 * SIZEOF_LIMB_T) + (e % (8 * SIZEOF_LIMB_T));
            }
        }
    }

    /* Now raise to the exponent */
    if ( r != 0.0 ) /* Lennart suggests this avoids a bug in MIPS's ldexp */
        r = ldexp(r, e);

    /* sign is encoded in the size */
    if (size < 0)
        r = -r;

    return r;
}

/* This only supports IEEE floating point */

void
integer_cbits_decodeDouble (MP_INT *man, I_ *exp, StgDouble dbl)
{
    /* Do some bit fiddling on IEEE */
    unsigned int low, high;                      /* assuming 32 bit ints */
    int sign, iexp;
    union { double d; unsigned int i[2]; } u;    /* assuming 32 bit ints, 64 bit double */

    ASSERT(sizeof(unsigned int ) == 4            );
    ASSERT(sizeof(dbl          ) == SIZEOF_DOUBLE);
    ASSERT(sizeof(man->_mp_d[0]) == SIZEOF_LIMB_T);
    ASSERT(DNBIGIT*SIZEOF_LIMB_T >= SIZEOF_DOUBLE);

    u.d = dbl;            /* grab chunks of the double */
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
        if (iexp != MY_DMINEXP)        /* don't add hidden bit to denorms */
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
