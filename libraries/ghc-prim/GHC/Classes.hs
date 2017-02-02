{-# LANGUAGE CPP, Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, StandaloneDeriving, BangPatterns,
             KindSignatures, DataKinds, ConstraintKinds,
              MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
  -- ip :: IP x a => a  is strictly speaking ambiguous, but IP is magic
{-# LANGUAGE UndecidableSuperClasses #-}
  -- Because of the type-variable superclasses for tuples

{-# OPTIONS_GHC -Wno-unused-imports #-}
-- -Wno-unused-imports needed for the GHC.Tuple import below. Sigh.

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- -Wno-unused-top-binds is there (I hope) to stop Haddock complaining
-- about the constraint tuples being defined but not used

{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Classes
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic classes.
--
-----------------------------------------------------------------------------

module GHC.Classes(
    -- * Implicit paramaters
    IP(..),

    -- * Equality and ordering
    Eq(..),
    Ord(..),
    -- ** Monomorphic equality operators
    -- | See GHC.Classes#matching_overloaded_methods_in_rules
    eqInt, neInt,
    eqWord, neWord,
    eqChar, neChar,
    eqFloat, eqDouble,
    -- ** Monomorphic comparison operators
    gtInt, geInt, leInt, ltInt, compareInt, compareInt#,
    gtWord, geWord, leWord, ltWord, compareWord, compareWord#,

    -- * Functions over Bool
    (&&), (||), not,

    -- * Integer arithmetic
    divInt#, modInt#
 ) where

-- GHC.Magic is used in some derived instances
import GHC.Magic ()
import GHC.IntWord64
import GHC.Prim
import GHC.Tuple
import GHC.Types

#include "MachDeps.h"

infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||

default ()              -- Double isn't available yet

-- | The syntax @?x :: a@ is desugared into @IP "x" a@
-- IP is declared very early, so that libraries can take
-- advantage of the implicit-call-stack feature
class IP (x :: Symbol) a | x -> a where
  ip :: a

{- $matching_overloaded_methods_in_rules

Matching on class methods (e.g. @(==)@) in rewrite rules tends to be a bit
fragile. For instance, consider this motivating example from the @bytestring@
library,

> break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
> breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
> {-# RULES "break -> breakByte" forall a. break (== x) = breakByte x #-}

Here we have two functions, with @breakByte@ providing an optimized
implementation of @break@ where the predicate is merely testing for equality
with a known @Word8@. As written, however, this rule will be quite fragile as
the @(==)@ class operation rule may rewrite the predicate before our @break@
rule has a chance to fire.

For this reason, most of the primitive types in @base@ have 'Eq' and 'Ord'
instances defined in terms of helper functions with inlinings delayed to phase
1. For instance, @Word8@\'s @Eq@ instance looks like,

> instance Eq Word8 where
>     (==) = eqWord8
>     (/=) = neWord8
>
> eqWord8, neWord8 :: Word8 -> Word8 -> Bool
> eqWord8 (W8# x) (W8# y) = ...
> neWord8 (W8# x) (W8# y) = ...
> {-# INLINE [1] eqWord8 #-}
> {-# INLINE [1] neWord8 #-}

This allows us to save our @break@ rule above by rewriting it to instead match
against @eqWord8@,

> {-# RULES "break -> breakByte" forall a. break (`eqWord8` x) = breakByte x #-}

Currently this is only done for '(==)', '(/=)', '(<)', '(<=)', '(>)', and '(>=)'
for the types in "GHC.Word" and "GHC.Int".
-}

-- | The 'Eq' class defines equality ('==') and inequality ('/=').
-- All the basic datatypes exported by the "Prelude" are instances of 'Eq',
-- and 'Eq' may be derived for any datatype whose constituents are also
-- instances of 'Eq'.
--
-- Minimal complete definition: either '==' or '/='.
--
class  Eq a  where
    (==), (/=)           :: a -> a -> Bool

    {-# INLINE (/=) #-}
    {-# INLINE (==) #-}
    x /= y               = not (x == y)
    x == y               = not (x /= y)
    {-# MINIMAL (==) | (/=) #-}

deriving instance Eq ()
deriving instance (Eq  a, Eq  b) => Eq  (a, b)
deriving instance (Eq  a, Eq  b, Eq  c) => Eq  (a, b, c)
deriving instance (Eq  a, Eq  b, Eq  c, Eq  d) => Eq  (a, b, c, d)
deriving instance (Eq  a, Eq  b, Eq  c, Eq  d, Eq  e) => Eq  (a, b, c, d, e)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f)
               => Eq (a, b, c, d, e, f)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g)
               => Eq (a, b, c, d, e, f, g)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h)
               => Eq (a, b, c, d, e, f, g, h)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i)
               => Eq (a, b, c, d, e, f, g, h, i)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j)
               => Eq (a, b, c, d, e, f, g, h, i, j)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k)
               => Eq (a, b, c, d, e, f, g, h, i, j, k)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k, Eq l)
               => Eq (a, b, c, d, e, f, g, h, i, j, k, l)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k, Eq l, Eq m)
               => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n)
               => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o)
               => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance (Eq a) => Eq [a] where
    {-# SPECIALISE instance Eq [[Char]] #-}
    {-# SPECIALISE instance Eq [Char] #-}
    {-# SPECIALISE instance Eq [Int] #-}
    []     == []     = True
    (x:xs) == (y:ys) = x == y && xs == ys
    _xs    == _ys    = False

deriving instance Eq Bool
deriving instance Eq Ordering

instance Eq Word where
    (==) = eqWord
    (/=) = neWord

-- See GHC.Classes#matching_overloaded_methods_in_rules
{-# INLINE [1] eqWord #-}
{-# INLINE [1] neWord #-}
eqWord, neWord :: Word -> Word -> Bool
(W# x) `eqWord` (W# y) = isTrue# (x `eqWord#` y)
(W# x) `neWord` (W# y) = isTrue# (x `neWord#` y)

-- See GHC.Classes#matching_overloaded_methods_in_rules
instance Eq Char where
    (==) = eqChar
    (/=) = neChar

-- See GHC.Classes#matching_overloaded_methods_in_rules
{-# INLINE [1] eqChar #-}
{-# INLINE [1] neChar #-}
eqChar, neChar :: Char -> Char -> Bool
(C# x) `eqChar` (C# y) = isTrue# (x `eqChar#` y)
(C# x) `neChar` (C# y) = isTrue# (x `neChar#` y)

instance Eq Float where
    (==) = eqFloat

-- See GHC.Classes#matching_overloaded_methods_in_rules
{-# INLINE [1] eqFloat #-}
eqFloat :: Float -> Float -> Bool
(F# x) `eqFloat` (F# y) = isTrue# (x `eqFloat#` y)

instance Eq Double where
    (==) = eqDouble

-- See GHC.Classes#matching_overloaded_methods_in_rules
{-# INLINE [1] eqDouble #-}
eqDouble :: Double -> Double -> Bool
(D# x) `eqDouble` (D# y) = isTrue# (x ==## y)

instance Eq Int where
    (==) = eqInt
    (/=) = neInt

-- See GHC.Classes#matching_overloaded_methods_in_rules
{-# INLINE [1] eqInt #-}
{-# INLINE [1] neInt #-}
eqInt, neInt :: Int -> Int -> Bool
(I# x) `eqInt` (I# y) = isTrue# (x ==# y)
(I# x) `neInt` (I# y) = isTrue# (x /=# y)

#if WORD_SIZE_IN_BITS < 64
instance Eq TyCon where
  (==) (TyCon hi1 lo1 _ _ _ _) (TyCon hi2 lo2 _ _ _ _)
       = isTrue# (hi1 `eqWord64#` hi2) && isTrue# (lo1 `eqWord64#` lo2)
instance Ord TyCon where
  compare (TyCon hi1 lo1 _ _ _ _) (TyCon hi2 lo2 _ _ _ _)
    | isTrue# (hi1 `gtWord64#` hi2) = GT
    | isTrue# (hi1 `ltWord64#` hi2) = LT
    | isTrue# (lo1 `gtWord64#` lo2) = GT
    | isTrue# (lo1 `ltWord64#` lo2) = LT
    | True                = EQ
#else
instance Eq TyCon where
  (==) (TyCon hi1 lo1 _ _ _ _) (TyCon hi2 lo2 _ _ _ _)
       = isTrue# (hi1 `eqWord#` hi2) && isTrue# (lo1 `eqWord#` lo2)
instance Ord TyCon where
  compare (TyCon hi1 lo1 _ _ _ _) (TyCon hi2 lo2 _ _ _ _)
    | isTrue# (hi1 `gtWord#` hi2) = GT
    | isTrue# (hi1 `ltWord#` hi2) = LT
    | isTrue# (lo1 `gtWord#` lo2) = GT
    | isTrue# (lo1 `ltWord#` lo2) = LT
    | True              = EQ
#endif


-- | The 'Ord' class is used for totally ordered datatypes.
--
-- Instances of 'Ord' can be derived for any user-defined
-- datatype whose constituent types are in 'Ord'.  The declared order
-- of the constructors in the data declaration determines the ordering
-- in derived 'Ord' instances.  The 'Ordering' datatype allows a single
-- comparison to determine the precise ordering of two objects.
--
-- Minimal complete definition: either 'compare' or '<='.
-- Using 'compare' can be more efficient for complex types.
--
class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a

    compare x y = if x == y then EQ
                  -- NB: must be '<=' not '<' to validate the
                  -- above claim about the minimal things that
                  -- can be defined for an instance of Ord:
                  else if x <= y then LT
                  else GT

    x <  y = case compare x y of { LT -> True;  _ -> False }
    x <= y = case compare x y of { GT -> False; _ -> True }
    x >  y = case compare x y of { GT -> True;  _ -> False }
    x >= y = case compare x y of { LT -> False; _ -> True }

        -- These two default methods use '<=' rather than 'compare'
        -- because the latter is often more expensive
    max x y = if x <= y then y else x
    min x y = if x <= y then x else y
    {-# MINIMAL compare | (<=) #-}

deriving instance Ord ()
deriving instance (Ord a, Ord b) => Ord (a, b)
deriving instance (Ord a, Ord b, Ord c) => Ord (a, b, c)
deriving instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (a, b, c, d, e)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f)
               => Ord (a, b, c, d, e, f)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g)
               => Ord (a, b, c, d, e, f, g)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h)
               => Ord (a, b, c, d, e, f, g, h)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i)
               => Ord (a, b, c, d, e, f, g, h, i)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j)
               => Ord (a, b, c, d, e, f, g, h, i, j)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k)
               => Ord (a, b, c, d, e, f, g, h, i, j, k)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k, Ord l)
               => Ord (a, b, c, d, e, f, g, h, i, j, k, l)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k, Ord l, Ord m)
               => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n)
               => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n, Ord o)
               => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance (Ord a) => Ord [a] where
    {-# SPECIALISE instance Ord [[Char]] #-}
    {-# SPECIALISE instance Ord [Char] #-}
    {-# SPECIALISE instance Ord [Int] #-}
    compare []     []     = EQ
    compare []     (_:_)  = LT
    compare (_:_)  []     = GT
    compare (x:xs) (y:ys) = case compare x y of
                                EQ    -> compare xs ys
                                other -> other

deriving instance Ord Bool
deriving instance Ord Ordering

-- We don't use deriving for Ord Char, because for Ord the derived
-- instance defines only compare, which takes two primops.  Then
-- '>' uses compare, and therefore takes two primops instead of one.
instance Ord Char where
    (C# c1) >  (C# c2) = isTrue# (c1 `gtChar#` c2)
    (C# c1) >= (C# c2) = isTrue# (c1 `geChar#` c2)
    (C# c1) <= (C# c2) = isTrue# (c1 `leChar#` c2)
    (C# c1) <  (C# c2) = isTrue# (c1 `ltChar#` c2)

instance Ord Float where
    (F# x) `compare` (F# y)
        = if      isTrue# (x `ltFloat#` y) then LT
          else if isTrue# (x `eqFloat#` y) then EQ
          else                                  GT

    (F# x) <  (F# y) = isTrue# (x `ltFloat#` y)
    (F# x) <= (F# y) = isTrue# (x `leFloat#` y)
    (F# x) >= (F# y) = isTrue# (x `geFloat#` y)
    (F# x) >  (F# y) = isTrue# (x `gtFloat#` y)

instance Ord Double where
    (D# x) `compare` (D# y)
        = if      isTrue# (x <##  y) then LT
          else if isTrue# (x ==## y) then EQ
          else                            GT

    (D# x) <  (D# y) = isTrue# (x <##  y)
    (D# x) <= (D# y) = isTrue# (x <=## y)
    (D# x) >= (D# y) = isTrue# (x >=## y)
    (D# x) >  (D# y) = isTrue# (x >##  y)

instance Ord Int where
    compare = compareInt
    (<)     = ltInt
    (<=)    = leInt
    (>=)    = geInt
    (>)     = gtInt

-- See GHC.Classes#matching_overloaded_methods_in_rules
{-# INLINE [1] gtInt #-}
{-# INLINE [1] geInt #-}
{-# INLINE [1] ltInt #-}
{-# INLINE [1] leInt #-}
gtInt, geInt, ltInt, leInt :: Int -> Int -> Bool
(I# x) `gtInt` (I# y) = isTrue# (x >#  y)
(I# x) `geInt` (I# y) = isTrue# (x >=# y)
(I# x) `ltInt` (I# y) = isTrue# (x <#  y)
(I# x) `leInt` (I# y) = isTrue# (x <=# y)

compareInt :: Int -> Int -> Ordering
(I# x#) `compareInt` (I# y#) = compareInt# x# y#

compareInt# :: Int# -> Int# -> Ordering
compareInt# x# y#
    | isTrue# (x# <#  y#) = LT
    | isTrue# (x# ==# y#) = EQ
    | True                = GT

instance Ord Word where
    compare = compareWord
    (<)     = ltWord
    (<=)    = leWord
    (>=)    = geWord
    (>)     = gtWord

-- See GHC.Classes#matching_overloaded_methods_in_rules
{-# INLINE [1] gtWord #-}
{-# INLINE [1] geWord #-}
{-# INLINE [1] ltWord #-}
{-# INLINE [1] leWord #-}
gtWord, geWord, ltWord, leWord :: Word -> Word -> Bool
(W# x) `gtWord` (W# y) = isTrue# (x `gtWord#` y)
(W# x) `geWord` (W# y) = isTrue# (x `geWord#` y)
(W# x) `ltWord` (W# y) = isTrue# (x `ltWord#` y)
(W# x) `leWord` (W# y) = isTrue# (x `leWord#` y)

compareWord :: Word -> Word -> Ordering
(W# x#) `compareWord` (W# y#) = compareWord# x# y#

compareWord# :: Word# -> Word# -> Ordering
compareWord# x# y#
    | isTrue# (x# `ltWord#` y#) = LT
    | isTrue# (x# `eqWord#` y#) = EQ
    | True                      = GT

-- OK, so they're technically not part of a class...:

-- Boolean functions

-- | Boolean \"and\"
(&&)                    :: Bool -> Bool -> Bool
True  && x              =  x
False && _              =  False

-- | Boolean \"or\"
(||)                    :: Bool -> Bool -> Bool
True  || _              =  True
False || x              =  x

-- | Boolean \"not\"
not                     :: Bool -> Bool
not True                =  False
not False               =  True


------------------------------------------------------------------------
-- These don't really belong here, but we don't have a better place to
-- put them

-- These functions have built-in rules.
{-# NOINLINE [0] divInt# #-}
{-# NOINLINE [0] modInt# #-}
divInt# :: Int# -> Int# -> Int#
x# `divInt#` y#
        -- Be careful NOT to overflow if we do any additional arithmetic
        -- on the arguments...  the following  previous version of this
        -- code has problems with overflow:
--    | (x# ># 0#) && (y# <# 0#) = ((x# -# y#) -# 1#) `quotInt#` y#
--    | (x# <# 0#) && (y# ># 0#) = ((x# -# y#) +# 1#) `quotInt#` y#
    =      if isTrue# (x# ># 0#) && isTrue# (y# <# 0#) then ((x# -# 1#) `quotInt#` y#) -# 1#
      else if isTrue# (x# <# 0#) && isTrue# (y# ># 0#) then ((x# +# 1#) `quotInt#` y#) -# 1#
      else x# `quotInt#` y#

modInt# :: Int# -> Int# -> Int#
x# `modInt#` y#
    = if isTrue# (x# ># 0#) && isTrue# (y# <# 0#) ||
         isTrue# (x# <# 0#) && isTrue# (y# ># 0#)
      then if isTrue# (r# /=# 0#) then r# +# y# else 0#
      else r#
    where
    !r# = x# `remInt#` y#


{- *************************************************************
*                                                              *
*               Constraint tuples                              *
*                                                              *
************************************************************* -}

class ()
class (c1, c2)     => (c1, c2)
class (c1, c2, c3) => (c1, c2, c3)
class (c1, c2, c3, c4) => (c1, c2, c3, c4)
class (c1, c2, c3, c4, c5) => (c1, c2, c3, c4, c5)
class (c1, c2, c3, c4, c5, c6) => (c1, c2, c3, c4, c5, c6)
class (c1, c2, c3, c4, c5, c6, c7) => (c1, c2, c3, c4, c5, c6, c7)
class (c1, c2, c3, c4, c5, c6, c7, c8) => (c1, c2, c3, c4, c5, c6, c7, c8)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17,c18)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59, c60)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59, c60)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59, c60, c61)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59, c60, c61)
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59, c60, c61, c62)
   => (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59, c60, c61, c62)
