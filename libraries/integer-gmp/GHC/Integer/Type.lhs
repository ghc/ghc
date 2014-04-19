\begin{code}
{-# LANGUAGE BangPatterns, CPP, UnboxedTuples, UnliftedFFITypes, MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

-- Commentary of Integer library is located on the wiki:
-- http://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/Integer
--
-- It gives an in-depth description of implementation details and
-- decisions.
--
-- TODO: Try to use optimized big/small int primops on IL32P64 archs
-- (mostly Windows/x86_64). Currently, we have to fall back to
-- unoptimized code-paths for most big/small-int primops, due to
-- @mpz_*()@ functions using @long@ types, which is smaller than
-- @mp_limb_t@ on IL32P64. The @mpn_*()@ functions are often safe to
-- use, as they use @mb_limb_t@ instead of @long@.
-- (look out for @#if SIZEOF_HSWORD == SIZEOF_LONG@ occurences)
--

#include "MachDeps.h"
#include "HsIntegerGmp.h"

#if SIZEOF_HSWORD == 4
#define INT_MINBOUND (-2147483648#)
#define NEG_INT_MINBOUND (S# 2147483647# `plusInteger` S# 1#)
#elif SIZEOF_HSWORD == 8
#define INT_MINBOUND (-9223372036854775808#)
#define NEG_INT_MINBOUND (S# 9223372036854775807# `plusInteger` S# 1#)
#else
#error Unknown SIZEOF_HSWORD; can't define INT_MINBOUND and NEG_INT_MINBOUND
#endif

module GHC.Integer.Type where

import GHC.Prim (
    -- Other types we use, convert from, or convert to
    Int#, Word#, Double#, Float#, ByteArray#, MutableByteArray#, Addr#, State#,
    indexIntArray#,
    -- Conversions between those types
    int2Word#, int2Double#, int2Float#, word2Int#,
    -- Operations on Int# that we use for operations on S#
    quotInt#, remInt#, quotRemInt#, negateInt#,
    (*#), (-#),
    (==#), (/=#), (<=#), (>=#), (<#), (>#),
    mulIntMayOflo#, addIntC#, subIntC#,
    and#, or#, xor#,
 )

import GHC.Integer.GMP.Prim (
    -- GMP-related primitives
    MPZ#,
    cmpInteger#, cmpIntegerInt#,
    plusInteger#, minusInteger#,
    timesInteger#,
    quotRemInteger#, quotInteger#, remInteger#,
    divModInteger#, divInteger#, modInteger#,
    divExactInteger#,
    gcdInteger#, gcdExtInteger#, gcdIntegerInt#, gcdInt#,
    decodeDouble#,
    int2Integer#, integer2Int#, word2Integer#, integer2Word#,
    andInteger#, orInteger#, xorInteger#, complementInteger#,
    testBitInteger#, mul2ExpInteger#, fdivQ2ExpInteger#,
    powInteger#, powModInteger#, powModSecInteger#, recipModInteger#,
    nextPrimeInteger#, testPrimeInteger#,
    sizeInBaseInteger#,
    importIntegerFromByteArray#, importIntegerFromAddr#,
    exportIntegerToMutableByteArray#, exportIntegerToAddr#,

#if SIZEOF_HSWORD == SIZEOF_LONG
    plusIntegerInt#, minusIntegerInt#,
    timesIntegerInt#,
    divIntegerWord#, modIntegerWord#, divModIntegerWord#,
    divExactIntegerWord#,
    quotIntegerWord#, remIntegerWord#, quotRemIntegerWord#,
#endif

#if WORD_SIZE_IN_BITS < 64
    int64ToInteger#,  integerToInt64#,
    word64ToInteger#, integerToWord64#,
#endif
 )

#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64 (
            Int64#, Word64#,
            int64ToWord64#, intToInt64#,
            int64ToInt#, word64ToInt64#,
            geInt64#, leInt64#, leWord64#,
       )
#endif

import GHC.Classes
import GHC.Types

default ()
\end{code}

%*********************************************************
%*                                                      *
\subsection{The @Integer@ type}
%*                                                      *
%*********************************************************

Convenient boxed Integer PrimOps.

\begin{code}
-- | Arbitrary-precision integers.
data Integer
   = S# Int#            -- ^ \"small\" integers fitting into an 'Int#'
   | J# Int# ByteArray# -- ^ \"big\" integers represented as GMP's @mpz_t@ structure.
     --
     -- The 'Int#' field corresponds to @mpz_t@'s @_mp_size@ field,
     -- which encodes the sign and the number of /limbs/ stored in the
     -- 'ByteArray#' field (i.e. @mpz_t@'s @_mp_d@ field). Note: The
     -- 'ByteArray#' may have been over-allocated, and thus larger
     -- than the size denoted by the 'Int#' field.
     --
     -- This representation tries to avoid using the GMP number
     -- representation for small integers that fit into a native
     -- 'Int#'. This allows to reduce (or at least defer) calling into GMP
     -- for operations whose results remain in the 'Int#'-domain.
     --
     -- Note: It does __not__ constitute a violation of invariants to
     -- represent an integer which would fit into an 'Int#' with the
     -- 'J#'-constructor. For instance, the value @0@ has (only) two valid
     -- representations, either @'S#' 0#@ or @'J#' 0 _@.

-- | Construct 'Integer' value from list of 'Int's.
--
-- This function is used by GHC for constructing 'Integer' literals.
mkInteger :: Bool   -- ^ sign of integer ('True' if non-negative)
          -> [Int]  -- ^ absolute value expressed in 31 bit chunks, least significant first

                    -- (ideally these would be machine-word 'Word's rather than 31-bit truncated 'Int's)
          -> Integer
mkInteger nonNegative is = let abs = f is
                           in if nonNegative then abs else negateInteger abs
    where f [] = S# 0#
          f (I# i : is') = S# i `orInteger` shiftLInteger (f is') 31#

{-# NOINLINE smallInteger #-}
smallInteger :: Int# -> Integer
smallInteger i = S# i

{-# NOINLINE wordToInteger #-}
wordToInteger :: Word# -> Integer
wordToInteger w = if isTrue# (i >=# 0#)
                  then S# i
                  else case word2Integer# w of (# s, d #) -> J# s d
    where
      !i = word2Int# w

{-# NOINLINE integerToWord #-}
integerToWord :: Integer -> Word#
integerToWord (S# i) = int2Word# i
integerToWord (J# s d) = integer2Word# s d

#if WORD_SIZE_IN_BITS < 64
{-# NOINLINE integerToWord64 #-}
integerToWord64 :: Integer -> Word64#
integerToWord64 (S# i) = int64ToWord64# (intToInt64# i)
integerToWord64 (J# s d) = integerToWord64# s d

{-# NOINLINE word64ToInteger #-}
word64ToInteger :: Word64# -> Integer
word64ToInteger w = if isTrue# (w `leWord64#` int64ToWord64# (intToInt64# 0x7FFFFFFF#))
                    then S# (int64ToInt# (word64ToInt64# w))
                    else case word64ToInteger# w of
                         (# s, d #) -> J# s d

{-# NOINLINE integerToInt64 #-}
integerToInt64 :: Integer -> Int64#
integerToInt64 (S# i) = intToInt64# i
integerToInt64 (J# s d) = integerToInt64# s d

{-# NOINLINE int64ToInteger #-}
int64ToInteger :: Int64# -> Integer
int64ToInteger i = if isTrue# (i `leInt64#` intToInt64# 0x7FFFFFFF#) &&
                      isTrue# (i `geInt64#` intToInt64# -0x80000000#)
                   then smallInteger (int64ToInt# i)
                   else case int64ToInteger# i of
                        (# s, d #) -> J# s d
#endif

integerToInt :: Integer -> Int#
{-# NOINLINE integerToInt #-}
integerToInt (S# i)   = i
integerToInt (J# s d) = integer2Int# s d

-- This manually floated out constant is needed as GHC doesn't do it on its own
minIntAsBig :: Integer
minIntAsBig = case int2Integer# INT_MINBOUND of { (# s, d #) -> J# s d }

-- | Promote 'S#' to 'J#'
toBig :: Integer -> Integer
toBig (S# i)     = case int2Integer# i of { (# s, d #) -> J# s d }
toBig i@(J# _ _) = i

-- | Demote 'J#' to 'S#' if possible. See also 'smartJ#'.
toSmall :: Integer -> Integer
toSmall i@(S# _)    = i
toSmall (J# s# mb#) = smartJ# s# mb#


-- | Smart 'J#' constructor which tries to construct 'S#' if possible
smartJ# :: Int# -> ByteArray# -> Integer
smartJ# 0# _ = S# 0#
smartJ# 1# mb#  | isTrue# (v ># 0#) = S# v
    where
      v = indexIntArray# mb# 0#
smartJ# (-1#) mb# | isTrue# (v <# 0#) = S# v
    where
      v = negateInt# (indexIntArray# mb# 0#)
smartJ# s# mb# = J# s# mb#

-- |Construct 'Integer' out of a 'MPZ#' as returned by GMP wrapper primops
--
-- IMPORTANT: The 'ByteArray#' element MUST NOT be accessed unless the
-- size-element indicates more than one limb!
--
-- See notes at definition site of 'MPZ#' in "GHC.Integer.GMP.Prim"
-- for more details.
mpzToInteger :: MPZ# -> Integer
mpzToInteger (# 0#, _, _ #) = S# 0#
mpzToInteger (# 1#, _, w# #) | isTrue# (v# >=# 0#) = S# v#
                             | True = case word2Integer# w# of (# _, d #) -> J# 1# d
    where
      v# = word2Int# w#
mpzToInteger (# -1#, _, w# #) | isTrue# (v# <=# 0#) = S# v#
                              | True = case word2Integer# w# of (# _, d #) -> J# -1# d
    where
      v# = negateInt# (word2Int# w#)
mpzToInteger (# s#, mb#, _ #) = J# s# mb#

-- | Variant of 'mpzToInteger' for pairs of 'Integer's
mpzToInteger2 :: (# MPZ#, MPZ# #) -> (# Integer, Integer #)
mpzToInteger2 (# mpz1, mpz2 #) = (# i1, i2 #)
    where
      !i1 = mpzToInteger mpz1 -- This use of `!` avoids creating thunks,
      !i2 = mpzToInteger mpz2 -- see also Note [Use S# if possible].

-- |Negate MPZ#
mpzNeg :: MPZ# -> MPZ#
mpzNeg (# s#, mb#, w# #) = (# negateInt# s#, mb#, w# #)

\end{code}

Note [Use S# if possible]
~~~~~~~~~~~~~~~~~~~~~~~~~
It's a big win to use S#, rather than J#, whenever possible.  Not only
does it take less space, but (probably more important) subsequent
operations are more efficient. See Trac #8638.

'smartJ#' is the smart constructor for J# that performs the necessary
tests.  When returning a nested result, we always use smartJ# strictly,
thus
       let !r = smartJ# a b in (# r, somthing_else #)
to avoid creating a thunk that is subsequently evaluated to a J#.
smartJ# itself does a pretty small amount of work, so it's not worth
thunking it.

We call 'smartJ#' in places like quotRemInteger where a big input
might produce a small output.

Just using smartJ# in this way has good results:

        Program           Size    Allocs   Runtime   Elapsed  TotalMem
--------------------------------------------------------------------------------
         gamteb          +0.1%    -19.0%      0.03      0.03     +0.0%
          kahan          +0.2%     -1.2%      0.17      0.17     +0.0%
         mandel          +0.1%     -7.7%      0.05      0.05     +0.0%
          power          +0.1%    -40.8%    -32.5%    -32.5%     +0.0%
         symalg          +0.2%     -0.5%      0.01      0.01     +0.0%
--------------------------------------------------------------------------------
            Min          +0.0%    -40.8%    -32.5%    -32.5%     -5.1%
            Max          +0.2%     +0.1%     +2.0%     +2.0%     +0.0%
 Geometric Mean          +0.1%     -1.0%     -2.5%     -2.5%     -0.1%

%*********************************************************
%*                                                      *
\subsection{Dividing @Integers@}
%*                                                      *
%*********************************************************

\begin{code}
-- XXX There's no good reason for us using unboxed tuples for the
-- results, but we don't have Data.Tuple available.

-- Note that we don't check for divide-by-zero here. That needs
-- to be done where it's used.
-- (we don't have error)

{-# NOINLINE quotRemInteger #-}
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger (S# INT_MINBOUND) b = quotRemInteger minIntAsBig b
quotRemInteger (S# i) (S# j) = case quotRemInt# i j of
                                   (# q, r #) -> (# S# q, S# r #)
#if SIZEOF_HSWORD == SIZEOF_LONG
quotRemInteger (J# s1 d1) (S# b) | isTrue# (b <# 0#)
  = case quotRemIntegerWord# s1 d1 (int2Word# (negateInt# b)) of
          (# q, r #) -> let !q' = mpzToInteger (mpzNeg q)
                            !r' = mpzToInteger r
                        in (# q', r' #) -- see also Trac #8726
quotRemInteger (J# s1 d1) (S# b)
  = mpzToInteger2 (quotRemIntegerWord# s1 d1 (int2Word# b))
#else
quotRemInteger i1@(J# _ _) i2@(S# _) = quotRemInteger i1 (toBig i2)
#endif
quotRemInteger i1@(S# _) i2@(J# _ _) = quotRemInteger (toBig i1) i2
quotRemInteger (J# s1 d1) (J# s2 d2)
  = mpzToInteger2(quotRemInteger# s1 d1 s2 d2) -- See Note [Use S# if possible]

{-# NOINLINE divModInteger #-}
divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger (S# INT_MINBOUND) b = divModInteger minIntAsBig b
divModInteger (S# i) (S# j) = (# S# d, S# m #)
    where
      -- NB. don't inline these.  (# S# (i `quotInt#` j), ... #) means
      -- (# let q = i `quotInt#` j in S# q, ... #) which builds a
      -- useless thunk.  Placing the bindings here means they'll be
      -- evaluated strictly.
      !d = i `divInt#` j
      !m = i `modInt#` j
#if SIZEOF_HSWORD == SIZEOF_LONG
divModInteger (J# s1 d1) (S# b) | isTrue# (b <# 0#)
  = case divModIntegerWord# (negateInt# s1) d1 (int2Word# (negateInt# b)) of
          (# q, r #) -> let !q' = mpzToInteger q
                            !r' = mpzToInteger (mpzNeg r)
                        in (# q', r' #) -- see also Trac #8726
divModInteger (J# s1 d1) (S# b)
  = mpzToInteger2(divModIntegerWord# s1 d1 (int2Word# b))
#else
divModInteger i1@(J# _ _) i2@(S# _) = divModInteger i1 (toBig i2)
#endif
divModInteger i1@(S# _) i2@(J# _ _) = divModInteger (toBig i1) i2
divModInteger (J# s1 d1) (J# s2 d2) = mpzToInteger2 (divModInteger# s1 d1 s2 d2)

{-# NOINLINE remInteger #-}
remInteger :: Integer -> Integer -> Integer
remInteger (S# INT_MINBOUND) b = remInteger minIntAsBig b
remInteger (S# a) (S# b) = S# (remInt# a b)
{- Special case doesn't work, because a 1-element J# has the range
   -(2^32-1) -- 2^32-1, whereas S# has the range -2^31 -- (2^31-1)
remInteger ia@(S# a) (J# sb b)
  | sb ==# 1#  = S# (remInt# a (word2Int# (integer2Word# sb b)))
  | sb ==# -1# = S# (remInt# a (0# -# (word2Int# (integer2Word# sb b))))
  | 0# <# sb   = ia
  | otherwise  = S# (0# -# a)
-}
remInteger ia@(S# _) ib@(J# _ _) = remInteger (toBig ia) ib
#if SIZEOF_HSWORD == SIZEOF_LONG
remInteger (J# sa a) (S# b)
  = mpzToInteger (remIntegerWord# sa a w)
  where
    w = int2Word# (if isTrue# (b <# 0#) then negateInt# b else b)
#else
remInteger i1@(J# _ _) i2@(S# _) = remInteger i1 (toBig i2)
#endif
remInteger (J# sa a) (J# sb b)
  = mpzToInteger (remInteger# sa a sb b)

{-# NOINLINE quotInteger #-}
quotInteger :: Integer -> Integer -> Integer
quotInteger (S# INT_MINBOUND) b = quotInteger minIntAsBig b
quotInteger (S# a) (S# b) = S# (quotInt# a b)
{- Special case disabled, see remInteger above
quotInteger (S# a) (J# sb b)
  | sb ==# 1#  = S# (quotInt# a (word2Int# (integer2Word# sb b)))
  | sb ==# -1# = S# (quotInt# a (0# -# (word2Int# (integer2Word# sb b))))
  | otherwise  = S# 0
-}
quotInteger ia@(S# _) ib@(J# _ _) = quotInteger (toBig ia) ib
#if SIZEOF_HSWORD == SIZEOF_LONG
quotInteger (J# sa a) (S# b) | isTrue# (b <# 0#)
  = mpzToInteger (mpzNeg (quotIntegerWord# sa a (int2Word# (negateInt# b))))
quotInteger (J# sa a) (S# b)
  = mpzToInteger (quotIntegerWord# sa a (int2Word# b))
#else
quotInteger i1@(J# _ _) i2@(S# _) = quotInteger i1 (toBig i2)
#endif
quotInteger (J# sa a) (J# sb b)
  = mpzToInteger (quotInteger# sa a sb b)

{-# NOINLINE modInteger #-}
modInteger :: Integer -> Integer -> Integer
modInteger (S# INT_MINBOUND) b = modInteger minIntAsBig b
modInteger (S# a) (S# b) = S# (modInt# a b)
modInteger ia@(S# _) ib@(J# _ _) = modInteger (toBig ia) ib
#if SIZEOF_HSWORD == SIZEOF_LONG
modInteger (J# sa a) (S# b) | isTrue# (b <# 0#)
  = mpzToInteger (mpzNeg (modIntegerWord# (negateInt# sa) a (int2Word# (negateInt# b))))
modInteger (J# sa a) (S# b)
  = mpzToInteger (modIntegerWord# sa a (int2Word# b))
#else
modInteger i1@(J# _ _) i2@(S# _) = modInteger i1 (toBig i2)
#endif
modInteger (J# sa a) (J# sb b)
  = mpzToInteger (modInteger# sa a sb b)

{-# NOINLINE divInteger #-}
divInteger :: Integer -> Integer -> Integer
divInteger (S# INT_MINBOUND) b = divInteger minIntAsBig b
divInteger (S# a) (S# b) = S# (divInt# a b)
divInteger ia@(S# _) ib@(J# _ _) = divInteger (toBig ia) ib
#if SIZEOF_HSWORD == SIZEOF_LONG
divInteger (J# sa a) (S# b) | isTrue# (b <# 0#)
  = mpzToInteger (divIntegerWord# (negateInt# sa) a (int2Word# (negateInt# b)))
divInteger (J# sa a) (S# b)
  = mpzToInteger (divIntegerWord# sa a (int2Word# b))
#else
divInteger i1@(J# _ _) i2@(S# _) = divInteger i1 (toBig i2)
#endif
divInteger (J# sa a) (J# sb b)
  = mpzToInteger (divInteger# sa a sb b)
\end{code}



\begin{code}
-- | Compute greatest common divisor.
{-# NOINLINE gcdInteger #-}
gcdInteger :: Integer -> Integer -> Integer
-- SUP: Do we really need the first two cases?
gcdInteger (S# INT_MINBOUND) b = gcdInteger minIntAsBig b
gcdInteger a (S# INT_MINBOUND) = gcdInteger a minIntAsBig
gcdInteger (S# a) (S# b) = S# (gcdInt a b)
gcdInteger ia@(S# a)  ib@(J# sb b)
 =      if isTrue# (a  ==# 0#) then absInteger ib
   else if isTrue# (sb ==# 0#) then absInteger ia
   else                             S# (gcdIntegerInt# absSb b absA)
       where !absA  = if isTrue# (a  <# 0#) then negateInt# a  else a
             !absSb = if isTrue# (sb <# 0#) then negateInt# sb else sb
gcdInteger ia@(J# _ _) ib@(S# _) = gcdInteger ib ia
gcdInteger (J# sa a) (J# sb b)   = mpzToInteger (gcdInteger# sa a sb b)

-- | Extended euclidean algorithm.
--
-- For @/a/@ and @/b/@, compute their greatest common divisor @/g/@
-- and the coefficient @/s/@ satisfying @/a//s/ + /b//t/ = /g/@.
--
-- /Since: 0.5.1.0/
{-# NOINLINE gcdExtInteger #-}
gcdExtInteger :: Integer -> Integer -> (# Integer, Integer #)
gcdExtInteger a@(S# _)   b@(S# _) = gcdExtInteger (toBig a) (toBig b)
gcdExtInteger a@(S# _) b@(J# _ _) = gcdExtInteger (toBig a) b
gcdExtInteger a@(J# _ _) b@(S# _) = gcdExtInteger a (toBig b)
gcdExtInteger (J# sa a) (J# sb b) = mpzToInteger2 (gcdExtInteger# sa a sb b)

-- | Compute least common multiple.
{-# NOINLINE lcmInteger #-}
lcmInteger :: Integer -> Integer -> Integer
lcmInteger a b =      if a `eqInteger` S# 0# then S# 0#
                 else if b `eqInteger` S# 0# then S# 0#
                 else (divExact aa (gcdInteger aa ab)) `timesInteger` ab
  where aa = absInteger a
        ab = absInteger b

-- | Compute greatest common divisor.
gcdInt :: Int# -> Int# -> Int#
gcdInt 0# y  = absInt y
gcdInt x  0# = absInt x
gcdInt x  y  = gcdInt# (absInt x) (absInt y)

absInt :: Int# -> Int#
absInt x = if isTrue# (x <# 0#) then negateInt# x else x

divExact :: Integer -> Integer -> Integer
divExact (S# INT_MINBOUND) b = divExact minIntAsBig b
divExact (S# a) (S# b) = S# (quotInt# a b)
divExact (S# a) (J# sb b)
  = S# (quotInt# a (integer2Int# sb b))
#if SIZEOF_HSWORD == SIZEOF_LONG
divExact (J# sa a) (S# b) | isTrue# (b <# 0#)
  = mpzToInteger (divExactIntegerWord# (negateInt# sa) a (int2Word# (negateInt# b)))
divExact (J# sa a) (S# b) = mpzToInteger (divExactIntegerWord# sa a (int2Word# b))
#else
divExact i1@(J# _ _) i2@(S# _) = divExact i1 (toBig i2)
#endif
divExact (J# sa a) (J# sb b) = mpzToInteger (divExactInteger# sa a sb b)
\end{code}


%*********************************************************
%*                                                      *
\subsection{The @Integer@ instances for @Eq@, @Ord@}
%*                                                      *
%*********************************************************

\begin{code}

-- | /Since: 0.5.1.0/
{-# NOINLINE eqInteger# #-}
eqInteger# :: Integer -> Integer -> Int#
eqInteger# (S# i)     (S# j)     = i ==# j
eqInteger# (S# i)     (J# s d)   = cmpIntegerInt# s d i ==# 0#
eqInteger# (J# s d)   (S# i)     = cmpIntegerInt# s d i ==# 0#
eqInteger# (J# s1 d1) (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) ==# 0#

-- | /Since: 0.5.1.0/
{-# NOINLINE neqInteger# #-}
neqInteger# :: Integer -> Integer -> Int#
neqInteger# (S# i)     (S# j)     = i /=# j
neqInteger# (S# i)     (J# s d)   = cmpIntegerInt# s d i /=# 0#
neqInteger# (J# s d)   (S# i)     = cmpIntegerInt# s d i /=# 0#
neqInteger# (J# s1 d1) (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) /=# 0#

{-# INLINE eqInteger  #-}
{-# INLINE neqInteger #-}
eqInteger, neqInteger :: Integer -> Integer -> Bool
eqInteger  a b = isTrue# (a `eqInteger#`  b)
neqInteger a b = isTrue# (a `neqInteger#` b)

instance  Eq Integer  where
    (==) = eqInteger
    (/=) = neqInteger

------------------------------------------------------------------------

-- | /Since: 0.5.1.0/
{-# NOINLINE leInteger# #-}
leInteger# :: Integer -> Integer -> Int#
leInteger# (S# i)     (S# j)     = i <=# j
leInteger# (J# s d)   (S# i)     = cmpIntegerInt# s d i <=# 0#
leInteger# (S# i)     (J# s d)   = cmpIntegerInt# s d i >=# 0#
leInteger# (J# s1 d1) (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) <=# 0#

-- | /Since: 0.5.1.0/
{-# NOINLINE gtInteger# #-}
gtInteger# :: Integer -> Integer -> Int#
gtInteger# (S# i)     (S# j)     = i ># j
gtInteger# (J# s d)   (S# i)     = cmpIntegerInt# s d i ># 0#
gtInteger# (S# i)     (J# s d)   = cmpIntegerInt# s d i <# 0#
gtInteger# (J# s1 d1) (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) ># 0#

-- | /Since: 0.5.1.0/
{-# NOINLINE ltInteger# #-}
ltInteger# :: Integer -> Integer -> Int#
ltInteger# (S# i)     (S# j)     = i <# j
ltInteger# (J# s d)   (S# i)     = cmpIntegerInt# s d i <# 0#
ltInteger# (S# i)     (J# s d)   = cmpIntegerInt# s d i ># 0#
ltInteger# (J# s1 d1) (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) <# 0#

-- | /Since: 0.5.1.0/
{-# NOINLINE geInteger# #-}
geInteger# :: Integer -> Integer -> Int#
geInteger# (S# i)     (S# j)     = i >=# j
geInteger# (J# s d)   (S# i)     = cmpIntegerInt# s d i >=# 0#
geInteger# (S# i)     (J# s d)   = cmpIntegerInt# s d i <=# 0#
geInteger# (J# s1 d1) (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) >=# 0#

{-# INLINE leInteger #-}
{-# INLINE ltInteger #-}
{-# INLINE geInteger #-}
{-# INLINE gtInteger #-}
leInteger, gtInteger, ltInteger, geInteger :: Integer -> Integer -> Bool
leInteger a b = isTrue# (a `leInteger#` b)
gtInteger a b = isTrue# (a `gtInteger#` b)
ltInteger a b = isTrue# (a `ltInteger#` b)
geInteger a b = isTrue# (a `geInteger#` b)

{-# NOINLINE compareInteger #-}
compareInteger :: Integer -> Integer -> Ordering
compareInteger (S# i)  (S# j)
   =      if isTrue# (i ==# j) then EQ
     else if isTrue# (i <=# j) then LT
     else                           GT
compareInteger (J# s d) (S# i)
   = case cmpIntegerInt# s d i of { res# ->
     if isTrue# (res# <# 0#) then LT else
     if isTrue# (res# ># 0#) then GT else EQ
     }
compareInteger (S# i) (J# s d)
   = case cmpIntegerInt# s d i of { res# ->
     if isTrue# (res# ># 0#) then LT else
     if isTrue# (res# <# 0#) then GT else EQ
     }
compareInteger (J# s1 d1) (J# s2 d2)
   = case cmpInteger# s1 d1 s2 d2 of { res# ->
     if isTrue# (res# <# 0#) then LT else
     if isTrue# (res# ># 0#) then GT else EQ
     }

instance Ord Integer where
    (<=) = leInteger
    (<)  = ltInteger
    (>)  = gtInteger
    (>=) = geInteger
    compare = compareInteger
\end{code}


%*********************************************************
%*                                                      *
\subsection{The @Integer@ instances for @Num@}
%*                                                      *
%*********************************************************

\begin{code}
{-# NOINLINE absInteger #-}
absInteger :: Integer -> Integer
absInteger (S# INT_MINBOUND) = NEG_INT_MINBOUND
absInteger n@(S# i)   = if isTrue# (i >=# 0#) then n else S# (negateInt# i)
absInteger n@(J# s d) = if isTrue# (s >=# 0#) then n else J# (negateInt# s) d

{-# NOINLINE signumInteger #-}
signumInteger :: Integer -> Integer
signumInteger (S# i) = if isTrue# (i <# 0#) then S# -1#
                       else if isTrue# (i ==# 0#) then S# 0#
                       else S# 1#
signumInteger (J# s d)
  = let
        !cmp = cmpIntegerInt# s d 0#
    in
    if      isTrue# (cmp >#  0#) then S# 1#
    else if isTrue# (cmp ==# 0#) then S# 0#
    else                              S# (negateInt# 1#)

{-# NOINLINE plusInteger #-}
plusInteger :: Integer -> Integer -> Integer
plusInteger (S# i)      (S# j)   = case addIntC# i j of
                                   (# r, c #) ->
                                       if isTrue# (c ==# 0#)
                                       then S# r
#if SIZEOF_HSWORD == SIZEOF_LONG
                                       else case int2Integer# i of
                                            (# s, d #) -> mpzToInteger (plusIntegerInt# s d j)
#else
                                       else plusInteger (toBig (S# i)) (toBig (S# j))
#endif
plusInteger i1@(J# _ _) (S# 0#)   = i1
#if SIZEOF_HSWORD == SIZEOF_LONG
plusInteger (J# s1 d1)  (S# j)    = mpzToInteger (plusIntegerInt# s1 d1 j)
#else
plusInteger i1@(J# _ _) i2@(S# _) = plusInteger i1 (toBig i2)
#endif
plusInteger i1@(S# _) i2@(J# _ _) = plusInteger i2 i1
plusInteger (J# s1 d1) (J# s2 d2) = mpzToInteger (plusInteger# s1 d1 s2 d2)

{-# NOINLINE minusInteger #-}
minusInteger :: Integer -> Integer -> Integer
minusInteger (S# i)      (S# j)    = case subIntC# i j of
                                     (# r, c #) ->
                                         if isTrue# (c ==# 0#) then S# r
#if SIZEOF_HSWORD == SIZEOF_LONG
                                         else case int2Integer# i of
                                              (# s, d #) -> mpzToInteger (minusIntegerInt# s d j)
#else
                                         else minusInteger (toBig (S# i)) (toBig (S# j))
#endif
minusInteger i1@(J# _ _) (S# 0#)   = i1
minusInteger (S# 0#)    (J# s2 d2) = J# (negateInt# s2) d2
#if SIZEOF_HSWORD == SIZEOF_LONG
minusInteger (J# s1 d1)  (S# j)    = mpzToInteger (minusIntegerInt# s1 d1 j)
minusInteger (S# i)     (J# s2 d2) = mpzToInteger (plusIntegerInt# (negateInt# s2) d2 i)
#else
minusInteger i1@(J# _ _) i2@(S# _) = minusInteger i1 (toBig i2)
minusInteger i1@(S# _) i2@(J# _ _) = minusInteger (toBig i1) i2
#endif
minusInteger (J# s1 d1) (J# s2 d2) = mpzToInteger (minusInteger# s1 d1 s2 d2)

{-# NOINLINE timesInteger #-}
timesInteger :: Integer -> Integer -> Integer
timesInteger (S# i) (S# j)         = if isTrue# (mulIntMayOflo# i j ==# 0#)
                                     then S# (i *# j)
#if SIZEOF_HSWORD == SIZEOF_LONG
                                     else case int2Integer# i of
                                          (# s, d #) -> mpzToInteger (timesIntegerInt# s d j)
#else
                                     else timesInteger (toBig (S# i)) (toBig (S# j))
#endif
timesInteger (S# 0#)     _         = S# 0#
timesInteger (S# -1#)    i2        = negateInteger i2
timesInteger (S# 1#)     i2        = i2
#if SIZEOF_HSWORD == SIZEOF_LONG
timesInteger (S# i1)    (J# s2 d2) = mpzToInteger (timesIntegerInt# s2 d2 i1)
#else
timesInteger i1@(S# _) i2@(J# _ _) = timesInteger (toBig i1) i2
#endif
timesInteger i1@(J# _ _) i2@(S# _) = timesInteger i2 i1 -- swap args & retry
timesInteger (J# s1 d1) (J# s2 d2) = mpzToInteger (timesInteger# s1 d1 s2 d2)

{-# NOINLINE negateInteger #-}
negateInteger :: Integer -> Integer
negateInteger (S# INT_MINBOUND) = NEG_INT_MINBOUND
negateInteger (S# i)            = S# (negateInt# i)
negateInteger (J# s d)          = J# (negateInt# s) d
\end{code}


%*********************************************************
%*                                                      *
\subsection{The @Integer@ stuff for Double@}
%*                                                      *
%*********************************************************

\begin{code}
{-# NOINLINE encodeFloatInteger #-}
encodeFloatInteger :: Integer -> Int# -> Float#
encodeFloatInteger (S# i) j     = int_encodeFloat# i j
encodeFloatInteger (J# s# d#) e = encodeFloat# s# d# e

{-# NOINLINE encodeDoubleInteger #-}
encodeDoubleInteger :: Integer -> Int# -> Double#
encodeDoubleInteger (S# i) j     = int_encodeDouble# i j
encodeDoubleInteger (J# s# d#) e = encodeDouble# s# d# e

{-# NOINLINE decodeDoubleInteger #-}
decodeDoubleInteger :: Double# -> (# Integer, Int# #)
decodeDoubleInteger d = case decodeDouble# d of
                        (# exp#, man# #) -> let !man = mpzToInteger man#
                                            in (# man, exp# #)

-- previous code: doubleFromInteger n = fromInteger n = encodeFloat n 0
-- doesn't work too well, because encodeFloat is defined in
-- terms of ccalls which can never be simplified away.  We
-- want simple literals like (fromInteger 3 :: Float) to turn
-- into (F# 3.0), hence the special case for S# here.

{-# NOINLINE doubleFromInteger #-}
doubleFromInteger :: Integer -> Double#
doubleFromInteger (S# i#) = int2Double# i#
doubleFromInteger (J# s# d#) = encodeDouble# s# d# 0#

{-# NOINLINE floatFromInteger #-}
floatFromInteger :: Integer -> Float#
floatFromInteger (S# i#) = int2Float# i#
floatFromInteger (J# s# d#) = encodeFloat# s# d# 0#

foreign import ccall unsafe "integer_cbits_encodeFloat"
        encodeFloat# :: Int# -> ByteArray# -> Int# -> Float#
foreign import ccall unsafe "__int_encodeFloat"
        int_encodeFloat# :: Int# -> Int# -> Float#

foreign import ccall unsafe "integer_cbits_encodeDouble"
        encodeDouble# :: Int# -> ByteArray# -> Int# -> Double#
foreign import ccall unsafe "__int_encodeDouble"
        int_encodeDouble# :: Int# -> Int# -> Double#
\end{code}

%*********************************************************
%*                                                      *
\subsection{The @Integer@ Bit definitions@}
%*                                                      *
%*********************************************************

We explicitly pattern match against J# and S# in order to produce
Core that doesn't have pattern matching errors, as that would
introduce a spurious dependency to base.

\begin{code}
{-# NOINLINE andInteger #-}
andInteger :: Integer -> Integer -> Integer
(S# x)     `andInteger`   (S# y)     = S# (word2Int# (int2Word# x `and#` int2Word# y))
x@(S# _)   `andInteger` y@(J# _ _)   = toBig x `andInteger` y
x@(J# _ _) `andInteger` y@(S# _)     = x `andInteger` toBig y
(J# s1 d1) `andInteger`   (J# s2 d2) =
     mpzToInteger (andInteger# s1 d1 s2 d2)

{-# NOINLINE orInteger #-}
orInteger :: Integer -> Integer -> Integer
(S# x)     `orInteger`   (S# y)     = S# (word2Int# (int2Word# x `or#` int2Word# y))
x@(S# _)   `orInteger` y@(J# _ _)   = toBig x `orInteger` y
x@(J# _ _) `orInteger` y@(S# _)     = x `orInteger` toBig y
(J# s1 d1) `orInteger`   (J# s2 d2) =
     mpzToInteger (orInteger# s1 d1 s2 d2)

{-# NOINLINE xorInteger #-}
xorInteger :: Integer -> Integer -> Integer
(S# x)     `xorInteger`   (S# y)     = S# (word2Int# (int2Word# x `xor#` int2Word# y))
x@(S# _)   `xorInteger` y@(J# _ _)   = toBig x `xorInteger` y
x@(J# _ _) `xorInteger` y@(S# _)     = x `xorInteger` toBig y
(J# s1 d1) `xorInteger`   (J# s2 d2) =
     mpzToInteger (xorInteger# s1 d1 s2 d2)

{-# NOINLINE complementInteger #-}
complementInteger :: Integer -> Integer
complementInteger (S# x)
    = S# (word2Int# (int2Word# x `xor#` int2Word# (0# -# 1#)))
complementInteger (J# s d)
    = mpzToInteger (complementInteger# s d)

{-# NOINLINE shiftLInteger #-}
shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger j@(S# _) i = shiftLInteger (toBig j) i
shiftLInteger (J# s d) i = mpzToInteger (mul2ExpInteger# s d i)

{-# NOINLINE shiftRInteger #-}
shiftRInteger :: Integer -> Int# -> Integer
shiftRInteger j@(S# _) i = shiftRInteger (toBig j) i
shiftRInteger (J# s d) i = mpzToInteger (fdivQ2ExpInteger# s d i)

-- | /Since: 0.5.1.0/
{-# NOINLINE testBitInteger #-}
testBitInteger :: Integer -> Int# -> Bool
testBitInteger j@(S# _) i = testBitInteger (toBig j) i
testBitInteger (J# s d) i = isTrue# (testBitInteger# s d i /=# 0#)

-- | \"@'powInteger' /b/ /e/@\" computes base @/b/@ raised to exponent @/e/@.
--
-- /Since: 0.5.1.0/
{-# NOINLINE powInteger #-}
powInteger :: Integer -> Word# -> Integer
powInteger j@(S# _) e = powInteger (toBig j) e
powInteger (J# s d) e = mpzToInteger (powInteger# s d e)

-- | \"@'powModInteger' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@.
--
-- Negative exponents are supported if an inverse modulo @/m/@
-- exists. It's advised to avoid calling this primitive with negative
-- exponents unless it is guaranteed the inverse exists, as failure to
-- do so will likely cause program abortion due to a divide-by-zero
-- fault. See also 'recipModInteger'.
--
-- /Since: 0.5.1.0/
{-# NOINLINE powModInteger #-}
powModInteger :: Integer -> Integer -> Integer -> Integer
powModInteger (J# s1 d1) (J# s2 d2) (J# s3 d3) =
    mpzToInteger (powModInteger# s1 d1 s2 d2 s3 d3)
powModInteger b e m = powModInteger (toBig b) (toBig e) (toBig m)

-- | \"@'powModSecInteger' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@. It is required that @/e/ > 0@ and
-- @/m/@ is odd.
--
-- This is a \"secure\" variant of 'powModInteger' using the
-- @mpz_powm_sec()@ function which is designed to be resilient to side
-- channel attacks and is therefore intended for cryptographic
-- applications.
--
-- This primitive is only available when the underlying GMP library
-- supports it (GMP >= 5). Otherwise, it internally falls back to
-- @'powModInteger'@, and a warning will be emitted when used.
--
-- /Since: 0.5.1.0/
{-# NOINLINE powModSecInteger #-}
powModSecInteger :: Integer -> Integer -> Integer -> Integer
powModSecInteger (J# s1 d1) (J# s2 d2) (J# s3 d3) =
    mpzToInteger (powModSecInteger# s1 d1 s2 d2 s3 d3)
powModSecInteger b e m = powModSecInteger (toBig b) (toBig e) (toBig m)

#if HAVE_SECURE_POWM == 0
{-# WARNING powModSecInteger "The underlying GMP library does not support a secure version of powModInteger which is side-channel resistant - you need at least GMP version 5 to support this" #-}
#endif

-- | \"@'recipModInteger' /x/ /m/@\" computes the inverse of @/x/@ modulo @/m/@. If
-- the inverse exists, the return value @/y/@ will satisfy @0 < /y/ <
-- abs(/m/)@, otherwise the result is @0@.
--
-- Note: The implementation exploits the undocumented property of
-- @mpz_invert()@ to not mangle the result operand (which is initialized
-- to 0) in case of non-existence of the inverse.
--
-- /Since: 0.5.1.0/
{-# NOINLINE recipModInteger #-}
recipModInteger :: Integer -> Integer -> Integer
recipModInteger j@(S# _) m@(S# _)   = recipModInteger (toBig j) (toBig m)
recipModInteger j@(S# _) m@(J# _ _) = recipModInteger (toBig j) m
recipModInteger j@(J# _ _) m@(S# _) = recipModInteger j (toBig m)
recipModInteger (J# s d) (J# ms md) = mpzToInteger (recipModInteger# s d ms md)

-- | Probalistic Miller-Rabin primality test.
--
-- \"@'testPrimeInteger' /n/ /k/@\" determines whether @/n/@ is prime
-- and returns one of the following results:
--
-- * @2#@ is returned if @/n/@ is definitely prime,
--
-- * @1#@ if @/n/@ is a /probable prime/, or
--
-- * @0#@ if @/n/@ is definitely not a prime.
--
-- The @/k/@ argument controls how many test rounds are performed for
-- determining a /probable prime/. For more details, see
-- <http://gmplib.org/manual/Number-Theoretic-Functions.html#index-mpz_005fprobab_005fprime_005fp-360 GMP documentation for `mpz_probab_prime_p()`>.
--
-- /Since: 0.5.1.0/
{-# NOINLINE testPrimeInteger #-}
testPrimeInteger :: Integer -> Int# -> Int#
testPrimeInteger j@(S# _) reps = testPrimeInteger (toBig j) reps
testPrimeInteger (J# s d) reps = testPrimeInteger# s d reps

-- | Compute next prime greater than @/n/@ probalistically.
--
-- According to the GMP documentation, the underlying function
-- @mpz_nextprime()@ \"uses a probabilistic algorithm to identify
-- primes. For practical purposes it's adequate, the chance of a
-- composite passing will be extremely small.\"
--
-- /Since: 0.5.1.0/
{-# NOINLINE nextPrimeInteger #-}
nextPrimeInteger :: Integer -> Integer
nextPrimeInteger j@(S# _) = nextPrimeInteger (toBig j)
nextPrimeInteger (J# s d) = mpzToInteger (nextPrimeInteger# s d)

-- | Compute number of digits (without sign) in given @/base/@.
--
-- It's recommended to avoid calling 'sizeInBaseInteger' for small
-- integers as this function would currently convert those to big
-- integers in order to call @mpz_sizeinbase()@.
--
-- This function wraps @mpz_sizeinbase()@ which has some
-- implementation pecularities to take into account:
--
-- * \"@'sizeInBaseInteger' 0 /base/ = 1@\" (see also comment in 'exportIntegerToMutableByteArray').
--
-- * This function is only defined if @/base/ >= 2#@ and @/base/ <= 256#@
--   (Note: the documentation claims that only @/base/ <= 62#@ is
--   supported, however the actual implementation supports up to base 256).
--
-- * If @/base/@ is a power of 2, the result will be exact. In other
--   cases (e.g. for @/base/ = 10#@), the result /may/ be 1 digit too large
--   sometimes.
--
-- * \"@'sizeInBaseInteger' /i/ 2#@\" can be used to determine the most
--   significant bit of @/i/@.
--
-- /Since: 0.5.1.0/
{-# NOINLINE sizeInBaseInteger #-}
sizeInBaseInteger :: Integer -> Int# -> Word#
sizeInBaseInteger (J# s d) b = sizeInBaseInteger# s d b
sizeInBaseInteger j@(S# _) b = sizeInBaseInteger (toBig j) b -- TODO

-- | Dump 'Integer' (without sign) to mutable byte-array in base-256 representation.
--
-- The call
--
-- @'exportIntegerToMutableByteArray' /i/ /mba/ /offset/ /order/@
--
-- writes
--
-- * the 'Integer' @/i/@
--
-- * into the 'MutableByteArray#' @/mba/@ starting at @/offset/@
--
-- * with most significant byte first if @order@ is @1#@ or least
--   significant byte first if @order@ is @-1#@, and
--
-- * returns number of bytes written.
--
-- Use \"@'sizeInBaseInteger' /i/ 256#@\" to compute the exact number of
-- bytes written in advance for @/i/ /= 0@. In case of @/i/ == 0@,
-- 'exportIntegerToMutableByteArray' will write and report zero bytes
-- written, whereas 'sizeInBaseInteger' report one byte.
--
-- It's recommended to avoid calling 'exportIntegerToMutableByteArray' for small
-- integers as this function would currently convert those to big
-- integers in order to call @mpz_export()@.
--
-- /Since: 0.5.1.0/
{-# NOINLINE exportIntegerToMutableByteArray #-}
exportIntegerToMutableByteArray :: Integer -> MutableByteArray# s -> Word# -> Int# -> State# s -> (# State# s, Word# #)
exportIntegerToMutableByteArray (J# s d) mba o e = exportIntegerToMutableByteArray# s d mba o e
exportIntegerToMutableByteArray j@(S# _) mba o e = exportIntegerToMutableByteArray (toBig j) mba o e -- TODO

-- | Dump 'Integer' (without sign) to @/addr/@ in base-256 representation.
--
-- @'exportIntegerToAddr' /addr/ /o/ /e/@
--
-- See description of 'exportIntegerToMutableByteArray' for more details.
--
-- /Since: 0.5.1.0/
{-# NOINLINE exportIntegerToAddr #-}
exportIntegerToAddr :: Integer -> Addr# -> Int# -> State# s -> (# State# s, Word# #)
exportIntegerToAddr (J# s d) addr o e = exportIntegerToAddr# s d addr o e
exportIntegerToAddr j@(S# _) addr o e = exportIntegerToAddr (toBig j) addr o e -- TODO

-- | Read 'Integer' (without sign) from byte-array in base-256 representation.
--
-- The call
--
-- @'importIntegerFromByteArray' /ba/ /offset/ /size/ /order/@
--
-- reads
--
-- * @/size/@ bytes from the 'ByteArray#' @/ba/@ starting at @/offset/@
--
-- * with most significant byte first if @/order/@ is @1#@ or least
--   significant byte first if @/order/@ is @-1#@, and
--
-- * returns a new 'Integer'
--
-- /Since: 0.5.1.0/
{-# NOINLINE importIntegerFromByteArray #-}
importIntegerFromByteArray :: ByteArray# -> Word# -> Word# -> Int# -> Integer
importIntegerFromByteArray ba o l e = mpzToInteger (importIntegerFromByteArray# ba o l e)

-- | Read 'Integer' (without sign) from memory location at @/addr/@ in
-- base-256 representation.
--
-- @'importIntegerFromAddr' /addr/ /size/ /order/@
--
-- See description of 'importIntegerFromByteArray' for more details.
--
-- /Since: 0.5.1.0/
{-# NOINLINE importIntegerFromAddr #-}
importIntegerFromAddr :: Addr# -> Word# -> Int# -> State# s -> (# State# s, Integer #)
importIntegerFromAddr addr l e st = case importIntegerFromAddr# addr l e st of
                                      (# st', mpz #) -> let !j = mpzToInteger mpz in (# st', j #)

\end{code}

%*********************************************************
%*                                                      *
\subsection{The @Integer@ hashing@}
%*                                                      *
%*********************************************************

\begin{code}
-- This is used by hashUnique

-- | 'hashInteger' returns the same value as 'fromIntegral', although in
-- unboxed form.  It might be a reasonable hash function for 'Integer',
-- given a suitable distribution of 'Integer' values.
--
-- Note: 'hashInteger' is currently just an alias for 'integerToInt'.

hashInteger :: Integer -> Int#
hashInteger = integerToInt
\end{code}

