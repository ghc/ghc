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
Miscellaneous primitive operations on StgInt64 and StgWord64s.
N.B. These are not primops!

Instead of going the normal (boring) route of making the list
of primitive operations even longer to cope with operations
over 64-bit entities, we implement them instead 'out-of-line'.

The primitive ops get their own routine (in C) that implements
the operation, requiring the caller to _ccall_ out. This has
performance implications of course, but we currently don't
expect intensive use of either Int64 or Word64 types.

The exceptions to the rule are primops that cast to and from
64-bit entities (these are defined in PrimOps.h)
*/

#include "Rts.h"

#ifdef SUPPORT_LONG_LONGS

/* Relational operators */

StgBool stg_gtWord64 (StgWord64 a, StgWord64 b) {return a >  b;}
StgBool stg_geWord64 (StgWord64 a, StgWord64 b) {return a >= b;}
StgBool stg_eqWord64 (StgWord64 a, StgWord64 b) {return a == b;}
StgBool stg_neWord64 (StgWord64 a, StgWord64 b) {return a != b;}
StgBool stg_ltWord64 (StgWord64 a, StgWord64 b) {return a <  b;}
StgBool stg_leWord64 (StgWord64 a, StgWord64 b) {return a <= b;}

StgBool stg_gtInt64 (StgInt64 a, StgInt64 b) {return a >  b;}
StgBool stg_geInt64 (StgInt64 a, StgInt64 b) {return a >= b;}
StgBool stg_eqInt64 (StgInt64 a, StgInt64 b) {return a == b;}
StgBool stg_neInt64 (StgInt64 a, StgInt64 b) {return a != b;}
StgBool stg_ltInt64 (StgInt64 a, StgInt64 b) {return a <  b;}
StgBool stg_leInt64 (StgInt64 a, StgInt64 b) {return a <= b;}

/* Arithmetic operators */

StgWord64 stg_remWord64  (StgWord64 a, StgWord64 b) {return a % b;}
StgWord64 stg_quotWord64 (StgWord64 a, StgWord64 b) {return a / b;}

StgInt64 stg_remInt64    (StgInt64 a, StgInt64 b)   {return a % b;}
StgInt64 stg_quotInt64   (StgInt64 a, StgInt64 b)   {return a / b;}
StgInt64 stg_negateInt64 (StgInt64 a)               {return -a;}
StgInt64 stg_plusInt64   (StgInt64 a, StgInt64 b)   {return a + b;}
StgInt64 stg_minusInt64  (StgInt64 a, StgInt64 b)   {return a - b;}
StgInt64 stg_timesInt64  (StgInt64 a, StgInt64 b)   {return a * b;}

/* Logical operators: */

StgWord64 stg_and64      (StgWord64 a, StgWord64 b) {return a & b;}
StgWord64 stg_or64       (StgWord64 a, StgWord64 b) {return a | b;}
StgWord64 stg_xor64      (StgWord64 a, StgWord64 b) {return a ^ b;}
StgWord64 stg_not64      (StgWord64 a)              {return ~a;}

StgWord64 stg_uncheckedShiftL64   (StgWord64 a, StgInt b)    {return a << b;}
StgWord64 stg_uncheckedShiftRL64  (StgWord64 a, StgInt b)    {return a >> b;}
/* Right shifting of signed quantities is not portable in C, so
   the behaviour you'll get from using these primops depends
   on the whatever your C compiler is doing. ToDo: fix. -- sof 8/98
*/
StgInt64  stg_uncheckedIShiftL64  (StgInt64 a,  StgInt b)    {return a << b;}
StgInt64  stg_uncheckedIShiftRA64 (StgInt64 a,  StgInt b)    {return a >> b;}
StgInt64  stg_uncheckedIShiftRL64 (StgInt64 a,  StgInt b)
                                    {return (StgInt64) ((StgWord64) a >> b);}

/* Casting between longs and longer longs.
   (the primops that cast from long longs to Integers
   expressed as macros, since these may cause some heap allocation).
*/

StgInt64  stg_intToInt64    (StgInt    i) {return (StgInt64)  i;}
StgInt    stg_int64ToInt    (StgInt64  i) {return (StgInt)    i;}
StgWord64 stg_int64ToWord64 (StgInt64  i) {return (StgWord64) i;}
StgWord64 stg_wordToWord64  (StgWord   w) {return (StgWord64) w;}
StgWord   stg_word64ToWord  (StgWord64 w) {return (StgWord)   w;}
StgInt64  stg_word64ToInt64 (StgWord64 w) {return (StgInt64)  w;}

StgWord64 stg_integerToWord64 (StgInt sa, StgByteArray /* Really: mp_limb_t* */ da)
{ 
  mp_limb_t* d;
  StgInt s;
  StgWord64 res;
  d = (mp_limb_t *)da;
  s = sa;
  switch (s) {
    case  0: res = 0;     break;
    case  1: res = d[0];  break;
    case -1: res = -(StgWord64)d[0]; break;
    default:
      res = (StgWord64)d[0] + ((StgWord64)d[1] << (BITS_IN (mp_limb_t)));
      if (s < 0) res = -res;
  }
  return res;
}

StgInt64 stg_integerToInt64 (StgInt sa, StgByteArray /* Really: mp_limb_t* */ da)
{ 
  mp_limb_t* d;
  StgInt s;
  StgInt64 res;
  d = (mp_limb_t *)da;
  s = (sa);
  switch (s) {
    case  0: res = 0;     break;
    case  1: res = d[0];  break;
    case -1: res = -(StgInt64)d[0]; break;
    default:
      res = (StgInt64)d[0] + ((StgWord64)d[1] << (BITS_IN (mp_limb_t)));
      if (s < 0) res = -res;
  }
  return res;
}

#endif /* SUPPORT_LONG_LONGS */
