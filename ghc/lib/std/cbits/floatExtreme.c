/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: floatExtreme.c,v 1.1 1998/04/10 10:54:28 simonm Exp $
 *
 * Stubs to check for extremities of (IEEE) floats, 
 * the tests have been (artfully) lifted from the hbc-0.9999.3 (lib/fltcode.c)
 * source.
 */

/*
ToDo:
  - avoid hard-wiring the fact that on an
    Alpha we repr. a StgFloat as a double.
    (introduce int equivalent of {ASSIGN,PK}_FLT? )
*/

#include "Rts.h"
#include "ieee-flpt.h"
#include "floatExtreme.h"

#ifdef BIGENDIAN
#define L 1
#define H 0
#else
#define L 0
#define H 1
#endif

#ifdef IEEE_FLOATING_POINT

StgInt
isDoubleNaN(StgDouble d)
{
    union { double d; int i[2]; } u;
    int hx,lx;
    int r;

    u.d = d;
    hx = u.i[H];
    lx = u.i[L];
    hx &= 0x7fffffff;
    hx |= (unsigned int)(lx|(-lx))>>31;        
    hx = 0x7ff00000 - hx;
    r = (int)((unsigned int)(hx))>>31;
    return (r);
}

StgInt
isDoubleInfinite(StgDouble d)
{
    union { double d; int i[2]; } u;
    int hx,lx;

    u.d = d;
    hx = u.i[H];
    lx = u.i[L];
    hx &= 0x7fffffff;
    hx ^= 0x7ff00000;
    hx |= lx;
    return (hx == 0);
}

StgInt
isDoubleDenormalized(StgDouble d) 
{
    union { double d; int i[2]; } u;
    int high, iexp;

    u.d = d;
    high = u.i[H];
    iexp = high & (0x7ff << 20);
    return (iexp == 0);
}

StgInt
isDoubleNegativeZero(StgDouble d) 
{
    union { double d; int i[2]; } u;
    int high, iexp;

    u.d = d;
    return (u.i[H] == 0x80000000 && u.i[L] == 0);
}

/* Same tests, this time for StgFloats. */

StgInt
isFloatNaN(StgFloat f)
{
#if !defined(alpha_TARGET_OS)
    /* StgFloat = double on alphas */
    return (isDoubleNaN(f));
#else
    union { StgFloat f; int i; } u;
    int r;
    u.f = f;

    u.i &= 0x7fffffff;
    u.i = 0x7f800000 - u.i;
    r = (int)(((unsigned int)(u.i))>>31);
    return (r);
#endif
}

StgInt
isFloatInfinite(StgFloat f)
{
#if !defined(alpha_TARGET_OS)
    /* StgFloat = double on alphas */
    return (isDoubleInfinite(f));
#else
    int ix;
    union { StgFloat f; int i; } u;
    u.f = f;

    u.i &= 0x7fffffff;
    u.i ^= 0x7f800000;
    return (u.i == 0);
#endif
}

StgInt
isFloatDenormalized(StgFloat f)
{
#if !defined(alpha_TARGET_OS)
    /* StgFloat = double on alphas */
    return (isDoubleDenormalized(f));
#else
    int iexp;
    union { StgFloat f; int i; } u;
    u.f = f;

    iexp = u.i & (0xff << 23);
    return (iexp == 0);
#endif
}

StgInt
isFloatNegativeZero(StgFloat f)
{
#if !defined(alpha_TARGET_OS)
    /* StgFloat = double on alphas */
    return (isDoubleNegativeZero(f));
#else
    union { StgFloat f; int i; } u;
    u.f = f;

    return (u.i  == (int)0x80000000);
#endif
}


#else

StgInt isDoubleNaN(d) StgDouble d; { return 0; }
StgInt isDoubleInfinite(d) StgDouble d; { return 0; }
StgInt isDoubleDenormalized(d) StgDouble d; { return 0; }
StgInt isDoubleNegativeZero(d) StgDouble d; { return 0; }
StgInt isFloatNaN(f) StgFloat f; { return 0; }
StgInt isFloatInfinite(f) StgFloat f; { return 0; }
StgInt isFloatDenormalized(f) StgFloat f; { return 0; }
StgInt isFloatNegativeZero(f) StgFloat f; { return 0; }

#endif
