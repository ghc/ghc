{-
(c) The University of Glasgow, 2000-2006

\section{Fast integers, etc... booleans moved to FastBool for using panic}
-}

{-# LANGUAGE CPP, MagicHash #-}

--Even if the optimizer could handle boxed arithmetic equally well,
--this helps automatically check the sources to make sure that
--it's only used in an appropriate pattern of efficiency.
--(it also makes `let`s and `case`s stricter...)

-- | Fast integers, characters and pointer types for use in many parts of GHC
module FastTypes (
    -- * FastInt
    FastInt,

    -- ** Getting in and out of FastInt
    _ILIT, iBox, iUnbox,

    -- ** Arithmetic on FastInt
    (+#), (-#), (*#), quotFastInt, negateFastInt,
    --quotRemFastInt is difficult because unboxed values can't
    --be tupled, but unboxed tuples aren't portable.  Just use
    -- nuisance boxed quotRem and rely on optimization.
    (==#), (/=#), (<#), (<=#), (>=#), (>#),
    minFastInt, maxFastInt,
    --prefer to distinguish operations, not types, between
    --signed and unsigned.
    --left-shift is the same for 'signed' and 'unsigned' numbers
    shiftLFastInt,
    --right-shift isn't the same for negative numbers (ones with
    --the highest-order bit '1').  If you don't care because the
    --number you're shifting is always nonnegative, use the '_' version
    --which should just be the fastest one.
    shiftR_FastInt,
    --"L' = logical or unsigned shift; 'A' = arithmetic or signed shift
    shiftRLFastInt, shiftRAFastInt,
    bitAndFastInt, bitOrFastInt,
    --add more operations to this file as you need them

    -- * FastChar
    FastChar,

    -- ** Getting in and out of FastChar
    _CLIT, cBox, cUnbox,

    -- ** Operations on FastChar
    fastOrd, fastChr, eqFastChar,
    --note, fastChr is "unsafe"Chr: it doesn't check for
    --character values above the range of Unicode

    -- * FastPtr
    FastPtr,

    -- ** Getting in and out of FastPtr
    pBox, pUnbox,

    -- ** Casting FastPtrs
    castFastPtr
  ) where

#include "HsVersions.h"

-- Import the beggars
import ExtsCompat46

type FastInt = Int#

--in case it's a macro, don't lexically feed an argument!
--e.g. #define _ILIT(x) (x#) , #define _ILIT(x) (x :: FastInt)
_ILIT = \(I# x) -> x
--perhaps for accomodating caseless-leading-underscore treatment,
--something like _iLIT or iLIT would be better?

iBox x = I# x
iUnbox (I# x) = x
quotFastInt   = quotInt#
negateFastInt = negateInt#

--I think uncheckedIShiftL# and uncheckedIShiftRL# are the same
--as uncheckedShiftL# and uncheckedShiftRL# ...
--should they be used? How new are they?
--They existed as far back as GHC 6.0 at least...
shiftLFastInt x y = uncheckedIShiftL# x y
shiftR_FastInt x y = uncheckedIShiftRL# x y
shiftRLFastInt x y = uncheckedIShiftRL# x y
shiftRAFastInt x y = uncheckedIShiftRA# x y
--{-# INLINE shiftLNonnegativeFastInt #-}
--{-# INLINE shiftRNonnegativeFastInt #-}
--shiftLNonnegativeFastInt n p = word2Int#((int2Word# n) `uncheckedShiftL#` p)
--shiftRNonnegativeFastInt n p = word2Int#((int2Word# n) `uncheckedShiftRL#` p)
bitAndFastInt x y = word2Int# (and# (int2Word# x) (int2Word# y))
bitOrFastInt x y = word2Int# (or# (int2Word# x) (int2Word# y))

type FastChar = Char#
_CLIT = \(C# c) -> c
cBox c = C# c
cUnbox (C# c) = c
fastOrd c = ord# c
fastChr x = chr# x
eqFastChar a b = eqChar# a b

--note that the type-parameter doesn't provide any safety
--when it's a synonym, but as long as we keep it compiling
--with and without __GLASGOW_HASKELL__ defined, it's fine.
type FastPtr a = Addr#
pBox p = Ptr p
pUnbox (Ptr p) = p
castFastPtr p = p

minFastInt, maxFastInt :: FastInt -> FastInt -> FastInt
minFastInt x y = if x <# y then x else y
maxFastInt x y = if x <# y then y else x

-- type-signatures will improve the non-ghc-specific versions
-- and keep things accurate (and ABLE to compile!)
_ILIT :: Int -> FastInt
iBox :: FastInt -> Int
iUnbox :: Int -> FastInt

quotFastInt :: FastInt -> FastInt -> FastInt
negateFastInt :: FastInt -> FastInt
shiftLFastInt, shiftR_FastInt, shiftRAFastInt, shiftRLFastInt
   :: FastInt -> FastInt -> FastInt
bitAndFastInt, bitOrFastInt :: FastInt -> FastInt -> FastInt

_CLIT :: Char -> FastChar
cBox :: FastChar -> Char
cUnbox :: Char -> FastChar
fastOrd :: FastChar -> FastInt
fastChr :: FastInt -> FastChar
eqFastChar :: FastChar -> FastChar -> Bool

pBox :: FastPtr a -> Ptr a
pUnbox :: Ptr a -> FastPtr a
castFastPtr :: FastPtr a -> FastPtr b
