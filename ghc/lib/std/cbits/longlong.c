/* -----------------------------------------------------------------------------
 * $Id: longlong.c,v 1.1 2001/07/13 11:03:47 rrt Exp $
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

#include "Stg.h"

#ifdef SUPPORT_LONG_LONGS

/* Relational operators */

StgBool gtWord64 (StgWord64 a, StgWord64 b) {return a >  b;}
StgBool geWord64 (StgWord64 a, StgWord64 b) {return a >= b;}
StgBool eqWord64 (StgWord64 a, StgWord64 b) {return a == b;}
StgBool neWord64 (StgWord64 a, StgWord64 b) {return a != b;}
StgBool ltWord64 (StgWord64 a, StgWord64 b) {return a <  b;}
StgBool leWord64 (StgWord64 a, StgWord64 b) {return a <= b;}

StgBool gtInt64 (StgInt64 a, StgInt64 b) {return a >  b;}
StgBool geInt64 (StgInt64 a, StgInt64 b) {return a >= b;}
StgBool eqInt64 (StgInt64 a, StgInt64 b) {return a == b;}
StgBool neInt64 (StgInt64 a, StgInt64 b) {return a != b;}
StgBool ltInt64 (StgInt64 a, StgInt64 b) {return a <  b;}
StgBool leInt64 (StgInt64 a, StgInt64 b) {return a <= b;}

/* Arithmetic operators */

StgWord64 remWord64  (StgWord64 a, StgWord64 b) {return a % b;}
StgWord64 quotWord64 (StgWord64 a, StgWord64 b) {return a / b;}
StgInt64 remInt64    (StgInt64 a, StgInt64 b)   {return a % b;}
StgInt64 quotInt64   (StgInt64 a, StgInt64 b)   {return a / b;}
StgInt64 negateInt64 (StgInt64 a)               {return -a;}
StgInt64 plusInt64   (StgInt64 a, StgInt64 b)   {return a + b;}
StgInt64 minusInt64  (StgInt64 a, StgInt64 b)   {return a - b;}
StgInt64 timesInt64  (StgInt64 a, StgInt64 b)   {return a * b;}

/* Logical operators: */

StgWord64 and64      (StgWord64 a, StgWord64 b) {return a & b;}
StgWord64 or64       (StgWord64 a, StgWord64 b) {return a | b;}
StgWord64 xor64      (StgWord64 a, StgWord64 b) {return a ^ b;}
StgWord64 not64      (StgWord64 a)              {return ~a;}
StgWord64 shiftL64   (StgWord64 a, StgInt b)    {return a << b;}
StgWord64 shiftRL64  (StgWord64 a, StgInt b)    {return a >> b;}
/* Right shifting of signed quantities is not portable in C, so
   the behaviour you'll get from using these primops depends
   on the whatever your C compiler is doing. ToDo: fix. -- sof 8/98
*/
StgInt64  iShiftL64  (StgInt64 a,  StgInt b)    {return a << b;}
StgInt64  iShiftRA64 (StgInt64 a,  StgInt b)    {return a >> b;}
StgInt64  iShiftRL64 (StgInt64 a,  StgInt b)
{return (StgInt64) ((StgWord64) a >> b);}

/* Casting between longs and longer longs:
   (the primops that cast between Integers and long longs are
   expressed as macros, since these may cause some heap allocation).
*/

StgInt64  intToInt64    (StgInt    i) {return (StgInt64)  i;}
StgInt    int64ToInt    (StgInt64  i) {return (StgInt)    i;}
StgWord64 int64ToWord64 (StgInt64  i) {return (StgWord64) i;}
StgWord64 wordToWord64  (StgWord   w) {return (StgWord64) w;}
StgWord   word64ToWord  (StgWord64 w) {return (StgWord)   w;}
StgInt64  word64ToInt64 (StgWord64 w) {return (StgInt64)  w;}

#endif /* SUPPORT_LONG_LONGS */
