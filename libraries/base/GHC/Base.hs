{-

The overall structure of the GHC Prelude is a bit tricky.

  a) We want to avoid "orphan modules", i.e. ones with instance
        decls that don't belong either to a tycon or a class
        defined in the same module

  b) We want to avoid giant modules

So the rough structure is as follows, in (linearised) dependency order


GHC.Prim        Has no implementation.  It defines built-in things, and
                by importing it you bring them into scope.
                The source file is GHC.Prim.hi-boot, which is just
                copied to make GHC.Prim.hi

GHC.Base        Classes: Eq, Ord, Functor, Monad
                Types:   List, (), Int, Bool, Ordering, Char, String

Data.Tuple      Types: tuples, plus instances for GHC.Base classes

GHC.Show        Class: Show, plus instances for GHC.Base/GHC.Tup types

GHC.Enum        Class: Enum,  plus instances for GHC.Base/GHC.Tup types

Data.Maybe      Type: Maybe, plus instances for GHC.Base classes

GHC.List        List functions

GHC.Num         Class: Num, plus instances for Int
                Type:  Integer, plus instances for all classes so far (Eq, Ord, Num, Show)

                Integer is needed here because it is mentioned in the signature
                of 'fromInteger' in class Num

GHC.Real        Classes: Real, Integral, Fractional, RealFrac
                         plus instances for Int, Integer
                Types:  Ratio, Rational
                        plus instances for classes so far

                Rational is needed here because it is mentioned in the signature
                of 'toRational' in class Real

GHC.ST  The ST monad, instances and a few helper functions

Ix              Classes: Ix, plus instances for Int, Bool, Char, Integer, Ordering, tuples

GHC.Arr         Types: Array, MutableArray, MutableVar

                Arrays are used by a function in GHC.Float

GHC.Float       Classes: Floating, RealFloat
                Types:   Float, Double, plus instances of all classes so far

                This module contains everything to do with floating point.
                It is a big module (900 lines)
                With a bit of luck, many modules can be compiled without ever reading GHC.Float.hi


Other Prelude modules are much easier with fewer complex dependencies.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}

-- -Wno-orphans is needed for things like:
-- Orphan rule: "x# -# x#" ALWAYS forall x# :: Int# -# x# x# = 0
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Base
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic data types and classes.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module GHC.Base
    ( module GHC.Base
    , module GHC.Base.FunOps
    , module GHC.Base.Functor
    , module GHC.Base.List
    , module GHC.Base.NonEmpty
    , module GHC.Base.Semigroup
    , module GHC.Base.String
    , module GHC.Base.Void
    , module GHC.Classes
    , module GHC.CString
    , module GHC.Magic
    , module GHC.Magic.Dict    , module GHC.Types
    , module GHC.Prim        -- Re-export GHC.Prim, GHC.Prim.Ext,
    , module GHC.Prim.Ext    -- GHC.Prim.PtrEq and [boot] GHC.Err
    , module GHC.Prim.PtrEq  -- to avoid lots of people having to
    , module GHC.Err         -- import these modules explicitly
    , module GHC.Maybe
    ) where

import GHC.Types
import GHC.Classes
import GHC.CString
import GHC.Magic
import GHC.Magic.Dict
import GHC.Prim
import GHC.Prim.Ext
import GHC.Prim.PtrEq
import GHC.Err
import GHC.Maybe

import GHC.Num.Integer ()        -- Note [Depend on GHC.Num.Integer]

-- See Note [Semigroup stimes cycle]
import {-# SOURCE #-} GHC.Num (Num (..))
import {-# SOURCE #-} GHC.Real (Integral (..))
import GHC.Base.FunOps
import GHC.Base.Functor
import GHC.Base.List
import GHC.Base.NonEmpty
import GHC.Base.Semigroup
import GHC.Base.String
import GHC.Base.Void

-- $setup
-- >>> import GHC.Num

default ()              -- Double isn't available yet

{-
Note [Depend on GHC.Num.Integer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Integer type is special because GHC.CoreToStg.Prep.mkConvertNumLiteral
lookups names in ghc-bignum interfaces to construct Integer literal values.
Currently it reads the interface file whether or not the current module *has*
any Integer literals, so it's important that GHC.Num.Integer is compiled before
any other module.

The danger is that if the build system doesn't know about the implicit
dependency on Integer, it'll compile some base module before GHC.Num.Integer,
resulting in:
  Failed to load interface for ‘GHC.Num.Integer’
    There are files missing in the ‘ghc-bignum’ package,

To ensure that GHC.Num.Integer is there, we must ensure that there is a visible
dependency on GHC.Num.Integer from every module in base.  We make GHC.Base
depend on GHC.Num.Integer; and everything else either depends on GHC.Base,
directly on GHC.Num.Integer, or does not have NoImplicitPrelude (and hence
depends on Prelude).

The lookup is only disabled for packages ghc-prim and ghc-bignum, which aren't
allowed to contain any Integer literal.


Note [Depend on GHC.Tuple]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Similarly, tuple syntax (or ()) creates an implicit dependency on
GHC.Tuple, so we use the same rule as for Integer --- see Note [Depend on
GHC.Num.Integer] --- to explain this to the build system.  We make GHC.Base
depend on GHC.Tuple, and everything else depends on GHC.Base or Prelude.

-}

#if 0
-- for use when compiling GHC.Base itself doesn't work
data  Bool  =  False | True
data Ordering = LT | EQ | GT
data Char = C# Char#
type  String = [Char]
data Int = I# Int#
data  ()  =  ()

not True = False
(&&) True True = True
otherwise = True
#endif

-- |'otherwise' is defined as the value 'True'.  It helps to make
-- guards more readable.  eg.
--
-- >  f x | x < 0     = ...
-- >      | otherwise = ...
otherwise               :: Bool
otherwise               =  True

----------------------------------------------
-- 'Int' related definitions
----------------------------------------------

maxInt, minInt :: Int

{- Seems clumsy. Should perhaps put minInt and MaxInt directly into MachDeps.h -}
#if WORD_SIZE_IN_BITS == 31
minInt  = I# (-0x40000000#)
maxInt  = I# 0x3FFFFFFF#
#elif WORD_SIZE_IN_BITS == 32
minInt  = I# (-0x80000000#)
maxInt  = I# 0x7FFFFFFF#
#else
minInt  = I# (-0x8000000000000000#)
maxInt  = I# 0x7FFFFFFFFFFFFFFF#
#endif

-- Assertion function.  This simply ignores its boolean argument.
-- The compiler may rewrite it to @('assertError' line)@.

-- | If the first argument evaluates to 'True', then the result is the
-- second argument.  Otherwise an 'Control.Exception.AssertionFailed' exception
-- is raised, containing a 'String' with the source file and line number of the
-- call to 'assert'.
--
-- Assertions can normally be turned on or off with a compiler flag
-- (for GHC, assertions are normally on unless optimisation is turned on
-- with @-O@ or the @-fignore-asserts@
-- option is given).  When assertions are turned off, the first
-- argument to 'assert' is ignored, and the second argument is
-- returned as the result.

--      SLPJ: in 5.04 etc 'assert' is in GHC.Prim,
--      but from Template Haskell onwards it's simply
--      defined here in Base.hs
assert :: Bool -> a -> a
assert _pred r = r

breakpoint :: a -> a
breakpoint r = r

breakpointCond :: Bool -> a -> a
breakpointCond _ r = r

data Opaque = forall a. O a


-- | @'until' p f@ yields the result of applying @f@ until @p@ holds.
until                   :: (a -> Bool) -> (a -> a) -> a -> a
until p f = go
  where
    go x | p x          = x
         | otherwise    = go (f x)

{- |
Returns the tag of a constructor application; this function is used
by the deriving code for Eq, Ord and Enum.
-}
{-# INLINE getTag #-}
getTag :: a -> Int#
getTag x = dataToTag# x

----------------------------------------------
-- Numeric primops
----------------------------------------------

-- Definitions of the boxed PrimOps; these will be
-- used in the case of partial applications, etc.

-- See Note [INLINE division wrappers]
{-# INLINE quotInt #-}
{-# INLINE remInt #-}
{-# INLINE divInt #-}
{-# INLINE modInt #-}
{-# INLINE quotRemInt #-}
{-# INLINE divModInt #-}

-- | Used to implement `quot` for the `Integral` typeclass.
--   This performs integer division on its two parameters, truncated towards zero.
--
-- ==== __Example__
-- >>> quotInt 10 2
-- 5
--
-- >>> quot 10 2
-- 5
quotInt :: Int -> Int -> Int
(I# x) `quotInt`  (I# y) = I# (x `quotInt#` y)
-- | Used to implement `rem` for the `Integral` typeclass.
--   This gives the remainder after integer division of its two parameters, satisfying
--
-- > ((x `quot` y) * y) + (x `rem` y) == x
--
-- ==== __Example__
-- >>> remInt 3 2
-- 1
--
-- >>> rem 3 2
-- 1
remInt  :: Int -> Int -> Int
(I# x) `remInt`   (I# y) = I# (x `remInt#`  y)
-- | Used to implement `div` for the `Integral` typeclass.
--   This performs integer division on its two parameters, truncated towards negative infinity.
--
-- ==== __Example__
-- >>> 10 `divInt` 2
-- 5
--
-- >>> 10 `div` 2
-- 5
divInt  :: Int -> Int -> Int
(I# x) `divInt`   (I# y) = I# (x `divInt#`  y)
-- | Used to implement `mod` for the `Integral` typeclass.
--   This performs the modulo operation, satisfying
--
-- > ((x `div` y) * y) + (x `mod` y) == x
--
-- ==== __Example__
-- >>> 7 `modInt` 3
-- 1
--
-- >>> 7 `mod` 3
-- 1
modInt  :: Int -> Int -> Int
(I# x) `modInt`   (I# y) = I# (x `modInt#`  y)


-- | Used to implement `quotRem` for the `Integral` typeclass.
--   This gives a tuple equivalent to
--
-- > (quot x y, mod x y)
--
-- ==== __Example__
-- >>> quotRemInt 10 2
-- (5,0)
--
-- >>> quotRem 10 2
-- (5,0)
quotRemInt :: Int -> Int -> (Int, Int)
(I# x) `quotRemInt` (I# y) = case x `quotRemInt#` y of
                             (# q, r #) ->
                                 (I# q, I# r)

-- | Used to implement `divMod` for the `Integral` typeclass.
--   This gives a tuple equivalent to
--
-- > (div x y, mod x y)
--
-- ==== __Example__
-- >>> divModInt 10 2
-- (5,0)
--
-- >>> divMod 10 2
-- (5,0)
divModInt :: Int -> Int -> (Int, Int)
(I# x) `divModInt` (I# y) = case x `divModInt#` y of
                            (# q, r #) -> (I# q, I# r)

{- Note [INLINE division wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Int division functions such as 'quotRemInt' and 'divModInt' have
been manually worker/wrappered, presumably because they construct
*nested* products.
We intend to preserve the exact worker/wrapper split, hence we mark
the wrappers INLINE (#19267). That makes sure the optimiser doesn't
accidentally inline the worker into the wrapper, undoing the manual
split again.
-}

-- Wrappers for the shift operations.  The uncheckedShift# family are
-- undefined when the amount being shifted by is greater than the size
-- in bits of Int#, so these wrappers perform a check and return
-- either zero or -1 appropriately.
--
-- Note that these wrappers still produce undefined results when the
-- second argument (the shift amount) is negative.

-- | This function is used to implement branchless shifts. If the number of bits
-- to shift is greater than or equal to the type size in bits, then the shift
-- must return 0.  Instead of doing a test, we use a mask obtained via this
-- function which is branchless too.
--
--    shift_mask m b
--      | b < m     = 0xFF..FF
--      | otherwise = 0
--
shift_mask :: Int# -> Int# -> Int#
{-# INLINE shift_mask #-}
shift_mask m b = negateInt# (b <# m)

-- | Shift the argument left by the specified number of bits
-- (which must be non-negative).
shiftL# :: Word# -> Int# -> Word#
a `shiftL#` b = (a `uncheckedShiftL#` b) `and#` int2Word# (shift_mask WORD_SIZE_IN_BITS# b)

-- | Shift the argument right by the specified number of bits
-- (which must be non-negative).
-- The "RL" means "right, logical" (as opposed to RA for arithmetic)
-- (although an arithmetic right shift wouldn't make sense for Word#)
shiftRL# :: Word# -> Int# -> Word#
a `shiftRL#` b = (a `uncheckedShiftRL#` b) `and#` int2Word# (shift_mask WORD_SIZE_IN_BITS# b)

-- | Shift the argument left by the specified number of bits
-- (which must be non-negative).
iShiftL# :: Int# -> Int# -> Int#
a `iShiftL#` b = (a `uncheckedIShiftL#` b) `andI#` shift_mask WORD_SIZE_IN_BITS# b

-- | Shift the argument right (signed) by the specified number of bits
-- (which must be non-negative).
-- The "RA" means "right, arithmetic" (as opposed to RL for logical)
iShiftRA# :: Int# -> Int# -> Int#
a `iShiftRA#` b | isTrue# (b >=# WORD_SIZE_IN_BITS#) = negateInt# (a <# 0#)
                | otherwise                          = a `uncheckedIShiftRA#` b

-- | Shift the argument right (unsigned) by the specified number of bits
-- (which must be non-negative).
-- The "RL" means "right, logical" (as opposed to RA for arithmetic)
iShiftRL# :: Int# -> Int# -> Int#
a `iShiftRL#` b = (a `uncheckedIShiftRL#` b) `andI#` shift_mask WORD_SIZE_IN_BITS# b

