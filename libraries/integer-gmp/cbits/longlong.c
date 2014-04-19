/* -----------------------------------------------------------------------------
 * $Id: longlong.c,v 1.4 2002/12/13 14:23:42 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Primitive operations over (64-bit) long longs
 * (only used on 32-bit platforms.)
 *
 * ---------------------------------------------------------------------------*/


/*
Primitive Integer conversions to/from HsInt64 and HsWord64s.
N.B. These are not primops!

Instead of going the normal (boring) route of making the list
of primitive operations even longer to cope with operations
over 64-bit entities, we implement them instead 'out-of-line'.

The primitive ops get their own routine (in C) that implements
the operation, requiring the caller to _ccall_ out. This has
performance implications of course, but we currently don't
expect intensive use of either Int64 or Word64 types.
*/

#include "Rts.h"

#if WORD_SIZE_IN_BITS < 64

HsWord64 hs_integerToWord64 (HsInt sa, StgByteArray /* Really: mp_limb_t* */ da)
{
  mp_limb_t* d;
  HsInt s;
  HsWord64 res;
  d = (mp_limb_t *)da;
  s = sa;
  switch (s) {
    case  0: res = 0;     break;
    case  1: res = d[0];  break;
    case -1: res = -(HsWord64)d[0]; break;
    default:
      res = (HsWord64)d[0] + ((HsWord64)d[1] << (BITS_IN (mp_limb_t)));
      if (s < 0) res = -res;
  }
  return res;
}

HsInt64 hs_integerToInt64 (HsInt sa, StgByteArray /* Really: mp_limb_t* */ da)
{
  mp_limb_t* d;
  HsInt s;
  HsInt64 res;
  d = (mp_limb_t *)da;
  s = (sa);
  switch (s) {
    case  0: res = 0;     break;
    case  1: res = d[0];  break;
    case -1: res = -(HsInt64)d[0]; break;
    default:
      res = (HsInt64)d[0] + ((HsWord64)d[1] << (BITS_IN (mp_limb_t)));
      if (s < 0) res = -res;
  }
  return res;
}

#endif /* WORD_SIZE_IN_BITS < 64 */
