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
