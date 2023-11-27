{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, StandaloneDeriving, BangPatterns,
             KindSignatures, DataKinds, ConstraintKinds,
              MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
  -- ip :: IP x a => a  is strictly speaking ambiguous, but IP is magic
{-# LANGUAGE UndecidableSuperClasses #-}
  -- Because of the type-variable superclasses for tuples

{-# OPTIONS_GHC -Wno-unused-imports #-}
-- -Wno-unused-imports needed for the GHC.Internal.Tuple import below. Sigh.

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- -Wno-unused-top-binds is there (I hope) to stop Haddock complaining
-- about the constraint tuples being defined but not used

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Classes
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic classes.
-- Do not import this module directly.  It is an GHC internal only
-- module.  Some of its contents are instead available from @Prelude@
-- and @GHC.Int@.
--
-----------------------------------------------------------------------------

module GHC.Internal.Classes(
    -- * Implicit parameters
    IP(..),

    -- * Equality and ordering
    -- | Do not import these classes from this module. Import them
    -- from @Prelude@ instead.
    Eq(..),
    Ord(..),
    -- ** Monomorphic equality operators
    -- $matching_overloaded_methods_in_rules
    eqInt, neInt,
    eqWord, neWord,
    eqChar, neChar,
    eqFloat, eqDouble,
    -- ** Monomorphic comparison operators
    gtInt, geInt, leInt, ltInt, compareInt, compareInt#,
    gtWord, geWord, leWord, ltWord, compareWord, compareWord#,

    -- * Functions over Bool
    -- | Do not import these functions from this module. Import them
    -- from @Prelude@ instead.
    (&&), (||), not,

    -- * Integer arithmetic
    divInt#, divInt8#, divInt16#, divInt32#,
    modInt#, modInt8#, modInt16#, modInt32#,
    divModInt#, divModInt8#, divModInt16#, divModInt32#,

    -- * Constraint tuples
    CUnit,
    CSolo,
    CTuple0,
    CTuple1,
    CTuple2,
    CTuple3,
    CTuple4,
    CTuple5,
    CTuple6,
    CTuple7,
    CTuple8,
    CTuple9,
    CTuple10,
    CTuple11,
    CTuple12,
    CTuple13,
    CTuple14,
    CTuple15,
    CTuple16,
    CTuple17,
    CTuple18,
    CTuple19,
    CTuple20,
    CTuple21,
    CTuple22,
    CTuple23,
    CTuple24,
    CTuple25,
    CTuple26,
    CTuple27,
    CTuple28,
    CTuple29,
    CTuple30,
    CTuple31,
    CTuple32,
    CTuple33,
    CTuple34,
    CTuple35,
    CTuple36,
    CTuple37,
    CTuple38,
    CTuple39,
    CTuple40,
    CTuple41,
    CTuple42,
    CTuple43,
    CTuple44,
    CTuple45,
    CTuple46,
    CTuple47,
    CTuple48,
    CTuple49,
    CTuple50,
    CTuple51,
    CTuple52,
    CTuple53,
    CTuple54,
    CTuple55,
    CTuple56,
    CTuple57,
    CTuple58,
    CTuple59,
    CTuple60,
    CTuple61,
    CTuple62,
    CTuple63,
    CTuple64,
 ) where

-- GHC.Internal.Magic is used in some derived instances
import GHC.Internal.Magic ()
import GHC.Internal.Magic.TagToEnum () -- also used in derived instances
import GHC.Internal.Prim
import GHC.Internal.Tuple
import GHC.Internal.CString (unpackCString#)
import GHC.Internal.Types

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

@
break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
\{\-\# RULES "break -> breakByte" forall a. break (== x) = breakByte x \#\-\}
@

Here we have two functions, with @breakByte@ providing an optimized
implementation of @break@ where the predicate is merely testing for equality
with a known @Word8@. As written, however, this rule will be quite fragile as
the @(==)@ class operation rule may rewrite the predicate before our @break@
rule has a chance to fire.

For this reason, most of the primitive types in @base@ have 'Eq' and 'Ord'
instances defined in terms of helper functions with inlinings delayed to phase
1. For instance, @Word8@\'s @Eq@ instance looks like,

@
instance Eq Word8 where
    (==) = eqWord8
    (/=) = neWord8

eqWord8, neWord8 :: Word8 -> Word8 -> Bool
eqWord8 (W8# x) (W8# y) = ...
neWord8 (W8# x) (W8# y) = ...
\{\-\# INLINE [1] eqWord8 \#\-\}
\{\-\# INLINE [1] neWord8 \#\-\}
@

This allows us to save our @break@ rule above by rewriting it to instead match
against @eqWord8@,

@
\{\-\# RULES "break -> breakByte" forall a. break (`eqWord8` x) = breakByte x \#\-\}
@

Currently this is only done for @('==')@, @('/=')@, @('<')@, @('<=')@, @('>')@,
and @('>=')@ for the types in "GHC.Word" and "GHC.Int".
-}

-- | The 'Eq' class defines equality ('==') and inequality ('/=').
-- All the basic datatypes exported by the "Prelude" are instances of 'Eq',
-- and 'Eq' may be derived for any datatype whose constituents are also
-- instances of 'Eq'.
--
-- The Haskell Report defines no laws for 'Eq'. However, instances are
-- encouraged to follow these properties:
--
-- [__Reflexivity__]: @x == x@ = 'True'
-- [__Symmetry__]: @x == y@ = @y == x@
-- [__Transitivity__]: if @x == y && y == z@ = 'True', then @x == z@ = 'True'
-- [__Extensionality__]: if @x == y@ = 'True' and @f@ is a function
-- whose return type is an instance of 'Eq', then @f x == f y@ = 'True'
-- [__Negation__]: @x /= y@ = @not (x == y)@
class  Eq a  where
    (==), (/=)           :: a -> a -> Bool

    {-# INLINE (/=) #-}
    {-# INLINE (==) #-}
    x /= y               = not (x == y)
    x == y               = not (x /= y)
    {-# MINIMAL (==) | (/=) #-}

deriving instance Eq ()
deriving instance Eq a => Eq (Solo a)
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

deriving instance Eq Module

instance Eq TrName where
    TrNameS a == TrNameS b = isTrue# (a `eqAddr#` b)
    a == b = toString a == toString b
      where
        toString (TrNameS s) = unpackCString# s
        toString (TrNameD s) = s

deriving instance Eq Bool
deriving instance Eq Ordering

instance Eq Word where
    (==) = eqWord
    (/=) = neWord

-- See GHC.Internal.Classes#matching_overloaded_methods_in_rules
{-# INLINE [1] eqWord #-}
{-# INLINE [1] neWord #-}
eqWord, neWord :: Word -> Word -> Bool
(W# x) `eqWord` (W# y) = isTrue# (x `eqWord#` y)
(W# x) `neWord` (W# y) = isTrue# (x `neWord#` y)

-- See GHC.Internal.Classes#matching_overloaded_methods_in_rules
instance Eq Char where
    (==) = eqChar
    (/=) = neChar

-- See GHC.Internal.Classes#matching_overloaded_methods_in_rules
{-# INLINE [1] eqChar #-}
{-# INLINE [1] neChar #-}
eqChar, neChar :: Char -> Char -> Bool
(C# x) `eqChar` (C# y) = isTrue# (x `eqChar#` y)
(C# x) `neChar` (C# y) = isTrue# (x `neChar#` y)

-- | Note that due to the presence of @NaN@, `Float`'s 'Eq' instance does not
-- satisfy reflexivity.
--
-- >>> 0/0 == (0/0 :: Float)
-- False
--
-- Also note that `Float`'s 'Eq' instance does not satisfy extensionality:
--
-- >>> 0 == (-0 :: Float)
-- True
-- >>> recip 0 == recip (-0 :: Float)
-- False
instance Eq Float where
    (==) = eqFloat

-- See GHC.Internal.Classes#matching_overloaded_methods_in_rules
{-# INLINE [1] eqFloat #-}
eqFloat :: Float -> Float -> Bool
(F# x) `eqFloat` (F# y) = isTrue# (x `eqFloat#` y)

-- | Note that due to the presence of @NaN@, `Double`'s 'Eq' instance does not
-- satisfy reflexivity.
--
-- >>> 0/0 == (0/0 :: Double)
-- False
--
-- Also note that `Double`'s 'Eq' instance does not satisfy substitutivity:
--
-- >>> 0 == (-0 :: Double)
-- True
-- >>> recip 0 == recip (-0 :: Double)
-- False
instance Eq Double where
    (==) = eqDouble

-- See GHC.Internal.Classes#matching_overloaded_methods_in_rules
{-# INLINE [1] eqDouble #-}
eqDouble :: Double -> Double -> Bool
(D# x) `eqDouble` (D# y) = isTrue# (x ==## y)

instance Eq Int where
    (==) = eqInt
    (/=) = neInt

-- See GHC.Internal.Classes#matching_overloaded_methods_in_rules
{-# INLINE [1] eqInt #-}
{-# INLINE [1] neInt #-}
eqInt, neInt :: Int -> Int -> Bool
(I# x) `eqInt` (I# y) = isTrue# (x ==# y)
(I# x) `neInt` (I# y) = isTrue# (x /=# y)

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


-- | The 'Ord' class is used for totally ordered datatypes.
--
-- Instances of 'Ord' can be derived for any user-defined datatype whose
-- constituent types are in 'Ord'. The declared order of the constructors in
-- the data declaration determines the ordering in derived 'Ord' instances. The
-- 'Ordering' datatype allows a single comparison to determine the precise
-- ordering of two objects.
--
-- 'Ord', as defined by the Haskell report, implements a total order and has the
-- following properties:
--
-- [__Comparability__]: @x <= y || y <= x@ = 'True'
-- [__Transitivity__]: if @x <= y && y <= z@ = 'True', then @x <= z@ = 'True'
-- [__Reflexivity__]: @x <= x@ = 'True'
-- [__Antisymmetry__]: if @x <= y && y <= x@ = 'True', then @x == y@ = 'True'
--
-- The following operator interactions are expected to hold:
--
-- 1. @x >= y@ = @y <= x@
-- 2. @x < y@ = @x <= y && x /= y@
-- 3. @x > y@ = @y < x@
-- 4. @x < y@ = @compare x y == LT@
-- 5. @x > y@ = @compare x y == GT@
-- 6. @x == y@ = @compare x y == EQ@
-- 7. @min x y == if x <= y then x else y@ = 'True'
-- 8. @max x y == if x >= y then x else y@ = 'True'
--
-- Note that (7.) and (8.) do /not/ require 'min' and 'max' to return either of
-- their arguments. The result is merely required to /equal/ one of the
-- arguments in terms of '(==)'. Users who expect a stronger guarantee are advised
-- to write their own min and/or max functions.
--
-- The nuance of the above distinction is not always fully internalized by
-- developers, and in the past (tracing back to the Haskell 1.4 Report) the
-- specification for 'Ord' asserted the stronger property that @(min x y, max x
-- y) = (x, y)@ or @(y, x)@, or in other words, that 'min' and 'max' /will/
-- return one of their arguments, using argument order as the tie-breaker if
-- the arguments are equal by comparison. A few list and
-- 'Data.Foldable.Foldable' functions have behavior that is best understood
-- with this assumption in mind: all variations of @minimumBy@ and @maximumBy@
-- (which can't use 'min' and 'max' in their implementations) are written such
-- that @minimumBy 'compare'@ and @maximumBy 'compare'@ are respectively
-- equivalent to @minimum@ and @maximum@ (which do use 'min' and 'max') only if
-- 'min' and 'max' adhere to this tie-breaking convention. Otherwise, if there
-- are multiple least or largest elements in a container, @minimum@ and
-- @maximum@ may not return the /same/ one that @minimumBy 'compare'@ and
-- @maximumBy 'compare'@ do (though they should return something that is
-- /equal/). (This is relevant for types with non-extensional equality, like
-- 'Data.Semigroup.Arg', but also in cases where the precise reference held
-- matters for memory-management reasons.) Unless there is a reason to deviate,
-- it is less confusing for implementors of 'Ord' to respect this same
-- convention (as the default definitions of 'min' and 'max' do).
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

    x <= y = case compare x y of { GT -> False; _ -> True }
    x >= y = y <= x
    x > y = not (x <= y)
    x < y = not (y <= x)


        -- These two default methods use '<=' rather than 'compare'
        -- because the latter is often more expensive
    max x y = if x <= y then y else x
    min x y = if x <= y then x else y
    {-# MINIMAL compare | (<=) #-}

deriving instance Ord ()
deriving instance Ord a => Ord (Solo a)
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

-- | See @instance@ 'Ord' 'Double' for discussion of deviations from IEEE 754 standard.
instance Ord Float where
    (F# x) `compare` (F# y)
        = if      isTrue# (x `ltFloat#` y) then LT
          else if isTrue# (x `eqFloat#` y) then EQ
          else                                  GT

    (F# x) <  (F# y) = isTrue# (x `ltFloat#` y)
    (F# x) <= (F# y) = isTrue# (x `leFloat#` y)
    (F# x) >= (F# y) = isTrue# (x `geFloat#` y)
    (F# x) >  (F# y) = isTrue# (x `gtFloat#` y)

-- | IEEE 754 'Double'-precision type includes not only numbers, but also
-- positive and negative infinities and a special element called @NaN@
-- (which can be quiet or signal).
--
-- IEEE 754-2008, section 5.11 requires that if at least one of arguments of
-- '<=', '<', '>', '>=' is @NaN@ then the result of the comparison is 'False',
-- and @instance@ 'Ord' 'Double' complies with this requirement. This violates
-- the reflexivity: both @NaN@ '<=' @NaN@ and @NaN@ '>=' @NaN@ are 'False'.
--
-- IEEE 754-2008, section 5.10 defines @totalOrder@ predicate. Unfortunately,
-- 'compare' on 'Double's violates the IEEE standard and does not define a total order.
-- More specifically, both 'compare' @NaN@ @x@ and 'compare' @x@ @NaN@ always return 'GT'.
--
-- Thus, users must be extremely cautious when using @instance@ 'Ord' 'Double'.
-- For instance, one should avoid ordered containers with keys represented by 'Double',
-- because data loss and corruption may happen. An IEEE-compliant 'compare' is available
-- in @fp-ieee@ package as @TotallyOrdered@ newtype.
--
-- Moving further, the behaviour of 'min' and 'max' with regards to @NaN@ is
-- also non-compliant. IEEE 754-2008, section 5.3.1 defines that quiet @NaN@
-- should be treated as a missing data by @minNum@ and @maxNum@ functions,
-- for example, @minNum(NaN, 1) = minNum(1, NaN) = 1@. Some languages such as Java
-- deviate from the standard implementing @minNum(NaN, 1) = minNum(1, NaN) = NaN@.
-- However, 'min' / 'max' in @base@ are even worse: 'min' @NaN@ 1 is 1, but 'min' 1 @NaN@
-- is @NaN@.
--
-- IEEE 754-2008 compliant 'min' / 'max' can be found in @ieee754@ package under
-- @minNum@ / @maxNum@ names. Implementations compliant with
-- @minimumNumber@ / @maximumNumber@ from a newer
-- [IEEE 754-2019](https://grouper.ieee.org/groups/msc/ANSI_IEEE-Std-754-2019/background/),
-- section 9.6 are available from @fp-ieee@ package.
--
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

-- See GHC.Internal.Classes#matching_overloaded_methods_in_rules
{-# INLINE [1] gtInt #-}
{-# INLINE [1] geInt #-}
{-# INLINE [1] ltInt #-}
{-# INLINE [1] leInt #-}
gtInt, geInt, ltInt, leInt :: Int -> Int -> Bool
(I# x) `gtInt` (I# y) = isTrue# (x >#  y)
(I# x) `geInt` (I# y) = isTrue# (x >=# y)
(I# x) `ltInt` (I# y) = isTrue# (x <#  y)
(I# x) `leInt` (I# y) = isTrue# (x <=# y)

-- See GHC.Internal.Classes#matching_overloaded_methods_in_rules
{-# INLINE [1] compareInt #-}
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

-- See GHC.Internal.Classes#matching_overloaded_methods_in_rules
{-# INLINE [1] gtWord #-}
{-# INLINE [1] geWord #-}
{-# INLINE [1] ltWord #-}
{-# INLINE [1] leWord #-}
gtWord, geWord, ltWord, leWord :: Word -> Word -> Bool
(W# x) `gtWord` (W# y) = isTrue# (x `gtWord#` y)
(W# x) `geWord` (W# y) = isTrue# (x `geWord#` y)
(W# x) `ltWord` (W# y) = isTrue# (x `ltWord#` y)
(W# x) `leWord` (W# y) = isTrue# (x `leWord#` y)

-- See GHC.Internal.Classes#matching_overloaded_methods_in_rules
{-# INLINE [1] compareWord #-}
compareWord :: Word -> Word -> Ordering
(W# x#) `compareWord` (W# y#) = compareWord# x# y#

compareWord# :: Word# -> Word# -> Ordering
compareWord# x# y#
    | isTrue# (x# `ltWord#` y#) = LT
    | isTrue# (x# `eqWord#` y#) = EQ
    | True                      = GT

-- OK, so they're technically not part of a class...:

-- Boolean functions

-- | Boolean \"and\", lazy in the second argument
(&&)                    :: Bool -> Bool -> Bool
True  && x              =  x
False && _              =  False

-- | Boolean \"or\", lazy in the second argument
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
{-# INLINE [0] divInt# #-}
divInt# :: Int# -> Int# -> Int#
x# `divInt#` y# = ((x# +# bias#) `quotInt#` y#) -# hard#
   where
      -- See Note [divInt# implementation]
      !yn#   = y# <# 0#
      !c0#   = (x# <# 0#) `andI#` (notI# yn#)
      !c1#   = (x# ># 0#) `andI#` yn#
      !bias# = c0# -# c1#
      !hard# = c0# `orI#` c1#

{-# INLINE [0] divInt8# #-}
divInt8# :: Int8# -> Int8# -> Int8#
x# `divInt8#` y# = ((x# `plusInt8#` bias#) `quotInt8#` y#) `subInt8#` hard#
   where
      zero# = intToInt8# 0#
      x `andInt8#` y = word8ToInt8# (int8ToWord8# x `andWord8#` int8ToWord8# y)
      x `orInt8#` y = word8ToInt8# (int8ToWord8# x `orWord8#` int8ToWord8# y)
      notInt8# x = word8ToInt8# (notWord8# (int8ToWord8# x))
      -- See Note [divInt# implementation]
      !yn#   = intToInt8# (y# `ltInt8#` zero#)
      !c0#   = intToInt8# (x# `ltInt8#` zero#) `andInt8#` (notInt8# yn#)
      !c1#   = intToInt8# (x# `gtInt8#` zero#) `andInt8#` yn#
      !bias# = c0# `subInt8#` c1#
      !hard# = c0# `orInt8#` c1#

{-# INLINE [0] divInt16# #-}
divInt16# :: Int16# -> Int16# -> Int16#
x# `divInt16#` y# = ((x# `plusInt16#` bias#) `quotInt16#` y#) `subInt16#` hard#
   where
      zero# = intToInt16# 0#
      x `andInt16#` y = word16ToInt16# (int16ToWord16# x `andWord16#` int16ToWord16# y)
      x `orInt16#` y = word16ToInt16# (int16ToWord16# x `orWord16#` int16ToWord16# y)
      notInt16# x = word16ToInt16# (notWord16# (int16ToWord16# x))
      -- See Note [divInt# implementation]
      !yn#   = intToInt16# (y# `ltInt16#` zero#)
      !c0#   = intToInt16# (x# `ltInt16#` zero#) `andInt16#` (notInt16# yn#)
      !c1#   = intToInt16# (x# `gtInt16#` zero#) `andInt16#` yn#
      !bias# = c0# `subInt16#` c1#
      !hard# = c0# `orInt16#` c1#

{-# INLINE [0] divInt32# #-}
divInt32# :: Int32# -> Int32# -> Int32#
x# `divInt32#` y# = ((x# `plusInt32#` bias#) `quotInt32#` y#) `subInt32#` hard#
   where
      zero# = intToInt32# 0#
      x `andInt32#` y = word32ToInt32# (int32ToWord32# x `andWord32#` int32ToWord32# y)
      x `orInt32#` y = word32ToInt32# (int32ToWord32# x `orWord32#` int32ToWord32# y)
      notInt32# x = word32ToInt32# (notWord32# (int32ToWord32# x))
      -- See Note [divInt# implementation]
      !yn#   = intToInt32# (y# `ltInt32#` zero#)
      !c0#   = intToInt32# (x# `ltInt32#` zero#) `andInt32#` (notInt32# yn#)
      !c1#   = intToInt32# (x# `gtInt32#` zero#) `andInt32#` yn#
      !bias# = c0# `subInt32#` c1#
      !hard# = c0# `orInt32#` c1#

-- Note [divInt# implementation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- divInt# (truncated toward zero) is implemented with quotInt# (truncated
-- toward negative infinity). They differ when inputs x and y have different signs:
--  - x `rem` y has the sign of x and (x `quot` y)*y + (x `rem` y) == x
--  - x `mod` y has the sign of y and (x `div`  y)*y + (x `mod` y) == x
--
-- So we bias the input and the result of quotInt as follows:
--
--         if isTrue# (x# ># 0#) && isTrue# (y# <# 0#) then ((x# -# 1#) `quotInt#` y#) -# 1#
--    else if isTrue# (x# <# 0#) && isTrue# (y# ># 0#) then ((x# +# 1#) `quotInt#` y#) -# 1#
--    else x# `quotInt#` y#
--
-- However this leads to assembly code with lots of branches (#19636) while we
-- would like simpler code that we could inline (#18067). So we use some
-- branchless code instead as derived below:
--
--         if isTrue# (x# ># 0#) && isTrue# (y# <# 0#) then ((x# -# 1#) `quotInt#` y#) -# 1#
--    else if isTrue# (x# <# 0#) && isTrue# (y# ># 0#) then ((x# +# 1#) `quotInt#` y#) -# 1#
--    else x# `quotInt#` y#
--
--  ===> { Give names to constants and always use them }
--
--    ((x# +# bias#) `quotInt#` y#) -# hard#
--      where
--        (bias#,hard#)
--          | isTrue# (x# ># 0#) && isTrue# (y# <# 0#) = (-1#, 1#)
--          | isTrue# (x# <# 0#) && isTrue# (y# ># 0#) = ( 1#, 1#)
--          | otherwise                                = ( 0#, 0#)
--
--  ===> { Compute bias# and hard# independently using Bool# (0#,1#) }
--
--    ((x# +# bias#) `quotInt#` y#) -# hard#
--      where
--        c0#   = (x# <# 0#) &&# (y# ># 0#)
--        c1#   = (x# ># 0#) &&# (y# <# 0#)
--        bias# = c0# -# c1#  -- both cases are mutually exclusive so we can subtract them
--        hard# = c0# ||# c1# -- (we could add them too here but OR is slightly better)
--
--  ===> { Use yn# variable for "y# <# 0#" }
--
--    ((x# +# bias#) `quotInt#` y#) -# hard#
--      where
--        -- y# ==# 0# throws an exception so we don't need to consider it
--        yn#   = y# <# 0#
--        c0#   = (x# <# 0#) &&# (notI# yn#)
--        c1#   = (x# ># 0#) &&# yn#
--        bias# = c0# -# c1#
--        hard# = c0# ||# c1#
--
--
-- Note that we need to be careful NOT to overflow if we do any additional
-- arithmetic on the arguments...  the following previous version of this code
-- had problems with overflow:
--    | (x# ># 0#) && (y# <# 0#) = ((x# -# y#) -# 1#) `quotInt#` y#
--    | (x# <# 0#) && (y# ># 0#) = ((x# -# y#) +# 1#) `quotInt#` y#

{-# INLINE [0] modInt# #-}
modInt# :: Int# -> Int# -> Int#
x# `modInt#` y# = r# +# k#
  where
    -- See Note [modInt# implementation]
    !yn# = y# <# 0#
    !c0# = (x# <# 0#) `andI#` (notI# yn#)
    !c1# = (x# ># 0#) `andI#` yn#
    !s#  = 0# -# ((c0# `orI#` c1#) `andI#` (r# /=# 0#))
    !k#  = s# `andI#` y#
    !r#  = x# `remInt#` y#

{-# INLINE [0] modInt8# #-}
modInt8# :: Int8# -> Int8# -> Int8#
x# `modInt8#` y# = r# `plusInt8#` k#
  where
    zero# = intToInt8# 0#
    x `andInt8#` y = word8ToInt8# (int8ToWord8# x `andWord8#` int8ToWord8# y)
    x `orInt8#` y = word8ToInt8# (int8ToWord8# x `orWord8#` int8ToWord8# y)
    notInt8# x = word8ToInt8# (notWord8# (int8ToWord8# x))
    -- See Note [modInt# implementation]
    !yn# = intToInt8# (y# `ltInt8#` zero#)
    !c0# = intToInt8# (x# `ltInt8#` zero#) `andInt8#` (notInt8# yn#)
    !c1# = intToInt8# (x# `gtInt8#` zero#) `andInt8#` yn#
    !s#  = zero# `subInt8#` ((c0# `orInt8#` c1#) `andInt8#` (intToInt8# (r# `neInt8#` zero#)))
    !k#  = s# `andInt8#` y#
    !r#  = x# `remInt8#` y#

{-# INLINE [0] modInt16# #-}
modInt16# :: Int16# -> Int16# -> Int16#
x# `modInt16#` y# = r# `plusInt16#` k#
  where
    zero# = intToInt16# 0#
    x `andInt16#` y = word16ToInt16# (int16ToWord16# x `andWord16#` int16ToWord16# y)
    x `orInt16#` y = word16ToInt16# (int16ToWord16# x `orWord16#` int16ToWord16# y)
    notInt16# x = word16ToInt16# (notWord16# (int16ToWord16# x))
    -- See Note [modInt# implementation]
    !yn# = intToInt16# (y# `ltInt16#` zero#)
    !c0# = intToInt16# (x# `ltInt16#` zero#) `andInt16#` (notInt16# yn#)
    !c1# = intToInt16# (x# `gtInt16#` zero#) `andInt16#` yn#
    !s#  = zero# `subInt16#` ((c0# `orInt16#` c1#) `andInt16#` (intToInt16# (r# `neInt16#` zero#)))
    !k#  = s# `andInt16#` y#
    !r#  = x# `remInt16#` y#

{-# INLINE [0] modInt32# #-}
modInt32# :: Int32# -> Int32# -> Int32#
x# `modInt32#` y# = r# `plusInt32#` k#
  where
    zero# = intToInt32# 0#
    x `andInt32#` y = word32ToInt32# (int32ToWord32# x `andWord32#` int32ToWord32# y)
    x `orInt32#` y = word32ToInt32# (int32ToWord32# x `orWord32#` int32ToWord32# y)
    notInt32# x = word32ToInt32# (notWord32# (int32ToWord32# x))
    -- See Note [modInt# implementation]
    !yn# = intToInt32# (y# `ltInt32#` zero#)
    !c0# = intToInt32# (x# `ltInt32#` zero#) `andInt32#` (notInt32# yn#)
    !c1# = intToInt32# (x# `gtInt32#` zero#) `andInt32#` yn#
    !s#  = zero# `subInt32#` ((c0# `orInt32#` c1#) `andInt32#` (intToInt32# (r# `neInt32#` zero#)))
    !k#  = s# `andInt32#` y#
    !r#  = x# `remInt32#` y#

-- Note [modInt# implementation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Similarly to divInt# (see Note [divInt# implementation]), we can derive the
-- branchless implementation of modInt# as follows:
--
--    = if isTrue# (x# ># 0#) && isTrue# (y# <# 0#) ||
--         isTrue# (x# <# 0#) && isTrue# (y# ># 0#)
--      then if isTrue# (r# /=# 0#) then r# +# y# else 0#
--      else r#
--    where
--     r# = x# `remInt#` y#
--
--  ===> { Introduce constant k# }
--
--    r# +# k#
--      where
--        k# = if isTrue# (x# ># 0#) && isTrue# (y# <# 0#) ||
--                isTrue# (x# <# 0#) && isTrue# (y# ># 0#)
--             then if isTrue# (r# /=# 0#) then y# else 0#
--             else 0#
--        r# = x# `remInt#` y#
--
--  ===> { Compute using Bool# }
--
--    r# +# k#
--      where
--        yn# = y# <# 0# -- we don't need to consider y# ==# 0#
--        c0# = (x# <# 0#) &&# (notI# yn#)
--        c1# = (x# ># 0#) &&# yn#
--        k#  = if isTrue# ((c0# ||# c1#) &&# (r# /=# 0#))
--                then y#
--                else 0#
--        r#  = x# `remInt#` y#
--
--  ===> { Select y# or 0# in branchless way }
--
--    r# +# k#
--      where
--        yn# = y# <# 0#
--        c0# = (x# <# 0#) &&# (notI# yn#)
--        c1# = (x# ># 0#) &&# yn#
--        -- s# is either equal to:
--        --    0#  (00..00b)
--        --    -1# (11..11b)
--        -- So we can AND s# with y#
--        s#  = 0# -# ((c0# ||# c1#) &&# (r# /=# 0#))
--        k#  = s# &&# y#
--        r#  = x# `remInt#` y#

{-# INLINE [0] divModInt# #-}
divModInt# :: Int# -> Int# -> (# Int#, Int# #)
x# `divModInt#` y# = case (x# +# bias#) `quotRemInt#` y# of
  (# q#, r# #) -> (# q# -# hard#, r# +# k# #)
  where
    -- See Note [divModInt# implementation]
    !yn#   = y# <# 0#
    !c0#   = (x# <# 0#) `andI#` (notI# yn#)
    !c1#   = (x# ># 0#) `andI#` yn#
    !bias# = c0# -# c1#
    !hard# = c0# `orI#` c1#
    !s#    = 0# -# hard#
    !k#    = (s# `andI#` y#) -# bias#

{-# INLINE [0] divModInt8# #-}
divModInt8# :: Int8# -> Int8# -> (# Int8#, Int8# #)
x# `divModInt8#` y# = case (x# `plusInt8#` bias#) `quotRemInt8#` y# of
  (# q#, r# #) -> (# q# `subInt8#` hard#, r# `plusInt8#` k# #)
  where
    zero# = intToInt8# 0#
    x `andInt8#` y = word8ToInt8# (int8ToWord8# x `andWord8#` int8ToWord8# y)
    x `orInt8#` y = word8ToInt8# (int8ToWord8# x `orWord8#` int8ToWord8# y)
    notInt8# x = word8ToInt8# (notWord8# (int8ToWord8# x))
    -- See Note [divModInt# implementation]
    !yn#   = intToInt8# (y# `ltInt8#` zero#)
    !c0#   = intToInt8# (x# `ltInt8#` zero#) `andInt8#` (notInt8# yn#)
    !c1#   = intToInt8# (x# `gtInt8#` zero#) `andInt8#` yn#
    !bias# = c0# `subInt8#` c1#
    !hard# = c0# `orInt8#` c1#
    !s#    = zero# `subInt8#` hard#
    !k#    = (s# `andInt8#` y#) `subInt8#` bias#

{-# INLINE [0] divModInt16# #-}
divModInt16# :: Int16# -> Int16# -> (# Int16#, Int16# #)
x# `divModInt16#` y# = case (x# `plusInt16#` bias#) `quotRemInt16#` y# of
  (# q#, r# #) -> (# q# `subInt16#` hard#, r# `plusInt16#` k# #)
  where
    zero# = intToInt16# 0#
    x `andInt16#` y = word16ToInt16# (int16ToWord16# x `andWord16#` int16ToWord16# y)
    x `orInt16#` y = word16ToInt16# (int16ToWord16# x `orWord16#` int16ToWord16# y)
    notInt16# x = word16ToInt16# (notWord16# (int16ToWord16# x))
    -- See Note [divModInt# implementation]
    !yn#   = intToInt16# (y# `ltInt16#` zero#)
    !c0#   = intToInt16# (x# `ltInt16#` zero#) `andInt16#` (notInt16# yn#)
    !c1#   = intToInt16# (x# `gtInt16#` zero#) `andInt16#` yn#
    !bias# = c0# `subInt16#` c1#
    !hard# = c0# `orInt16#` c1#
    !s#    = zero# `subInt16#` hard#
    !k#    = (s# `andInt16#` y#) `subInt16#` bias#

{-# INLINE [0] divModInt32# #-}
divModInt32# :: Int32# -> Int32# -> (# Int32#, Int32# #)
x# `divModInt32#` y# = case (x# `plusInt32#` bias#) `quotRemInt32#` y# of
  (# q#, r# #) -> (# q# `subInt32#` hard#, r# `plusInt32#` k# #)
  where
    zero# = intToInt32# 0#
    x `andInt32#` y = word32ToInt32# (int32ToWord32# x `andWord32#` int32ToWord32# y)
    x `orInt32#` y = word32ToInt32# (int32ToWord32# x `orWord32#` int32ToWord32# y)
    notInt32# x = word32ToInt32# (notWord32# (int32ToWord32# x))
    -- See Note [divModInt# implementation]
    !yn#   = intToInt32# (y# `ltInt32#` zero#)
    !c0#   = intToInt32# (x# `ltInt32#` zero#) `andInt32#` (notInt32# yn#)
    !c1#   = intToInt32# (x# `gtInt32#` zero#) `andInt32#` yn#
    !bias# = c0# `subInt32#` c1#
    !hard# = c0# `orInt32#` c1#
    !s#    = zero# `subInt32#` hard#
    !k#    = (s# `andInt32#` y#) `subInt32#` bias#

-- Note [divModInt# implementation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- divModInt# is written by deriving the following code similarly to divInt# and
-- modInt# (see Note [divInt# implementation] and Note [modInt#
-- implementation]).
--
--    x# `divModInt#` y#
--     | isTrue# (x# ># 0#) && isTrue# (y# <# 0#) =
--                                        case (x# -# 1#) `quotRemInt#` y# of
--                                          (# q, r #) -> (# q -# 1#, r +# y# +# 1# #)
--     | isTrue# (x# <# 0#) && isTrue# (y# ># 0#) =
--                                        case (x# +# 1#) `quotRemInt#` y# of
--                                          (# q, r #) -> (# q -# 1#, r +# y# -# 1# #)
--     | otherwise                                =
--                                        x# `quotRemInt#` y#
--
--  ===> { Introduce constants }
--
--    case (x# +# bias#) `quotRemInt#` y# of
--      (# q#, r# #) -> (# q# -# hard#, r# +# k# #)
--      where
--       (bias#,hard#,k#)
--        | isTrue# (x# ># 0#) && isTrue# (y# <# 0#) = (-1#, 1#, y#+1#)
--        | isTrue# (x# <# 0#) && isTrue# (y# ># 0#) = ( 1#, 1#, y#-1#)
--        | otherwise                                = ( 0#, 0#, 0#-0#)
--
--  ===> { Compute using Bool# }
--
--    case (x# +# bias#) `quotRemInt#` y# of
--      (# q#, r# #) -> (# q# -# hard#, r# +# k# #)
--      where
--        yn#   = y# <# 0#
--        c0#   = (x# <# 0#) `andI#` (notI# yn#)
--        c1#   = (x# ># 0#) `andI#` yn#
--        bias# = c0# -# c1#
--        hard# = c0# `orI#` c1#
--        s#    = 0# -# hard#
--        k#    = (s# `andI#` y#) -# bias#
--

{- *************************************************************
*                                                              *
*               Constraint tuples                              *
*                                                              *
************************************************************* -}

type CTuple0 = (() :: Constraint)
type CTuple1 = CSolo

class CUnit
class a => CSolo a
class (c1, c2) => CTuple2 c1 c2
class (c1, c2, c3) => CTuple3 c1 c2 c3
class (c1, c2, c3, c4) => CTuple4 c1 c2 c3 c4
class (c1, c2, c3, c4, c5) => CTuple5 c1 c2 c3 c4 c5
class (c1, c2, c3, c4, c5, c6) => CTuple6 c1 c2 c3 c4 c5 c6
class (c1, c2, c3, c4, c5, c6, c7) => CTuple7 c1 c2 c3 c4 c5 c6 c7
class (c1, c2, c3, c4, c5, c6, c7, c8) => CTuple8 c1 c2 c3 c4 c5 c6 c7 c8
class (c1, c2, c3, c4, c5, c6, c7, c8, c9)
   => CTuple9 c1 c2 c3 c4 c5 c6 c7 c8 c9
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
   => CTuple10 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
   => CTuple11 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
   => CTuple12 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
   => CTuple13 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
   => CTuple14 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
   => CTuple15 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
   => CTuple16 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17)
   => CTuple17 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17,c18)
   => CTuple18 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19)
   => CTuple19 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20)
   => CTuple20 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21)
   => CTuple21 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22)
   => CTuple22 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23)
   => CTuple23 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24)
   => CTuple24 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25)
   => CTuple25 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26)
   => CTuple26 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27)
   => CTuple27 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28)
   => CTuple28 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29)
   => CTuple29 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30)
   => CTuple30 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31)
   => CTuple31 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32)
   => CTuple32 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33)
   => CTuple33 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34)
   => CTuple34 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35)
   => CTuple35 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36)
   => CTuple36 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37)
   => CTuple37 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38)
   => CTuple38 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39)
   => CTuple39 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40)
   => CTuple40 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41)
   => CTuple41 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42)
   => CTuple42 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43)
   => CTuple43 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44)
   => CTuple44 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45)
   => CTuple45 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46)
   => CTuple46 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47)
   => CTuple47 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48)
   => CTuple48 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49)
   => CTuple49 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50)
   => CTuple50 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51)
   => CTuple51 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52)
   => CTuple52 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53)
   => CTuple53 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54)
   => CTuple54 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55)
   => CTuple55 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56)
   => CTuple56 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57)
   => CTuple57 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56 c57
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58)
   => CTuple58 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56 c57 c58
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59)
   => CTuple59 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56 c57 c58 c59
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59, c60)
   => CTuple60 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56 c57 c58 c59 c60
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59, c60, c61)
   => CTuple61 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56 c57 c58 c59 c60 c61
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59, c60, c61, c62)
   => CTuple62 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56 c57 c58 c59 c60 c61 c62
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59, c60, c61, c62, c63)
   => CTuple63 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56 c57 c58 c59 c60 c61 c62 c63
class (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59, c60, c61, c62, c63, c64)
   => CTuple64 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56 c57 c58 c59 c60 c61 c62 c63 c64

instance CUnit
instance a => CSolo a
instance (c1, c2) => CTuple2 c1 c2
instance (c1, c2, c3) => CTuple3 c1 c2 c3
instance (c1, c2, c3, c4) => CTuple4 c1 c2 c3 c4
instance (c1, c2, c3, c4, c5) => CTuple5 c1 c2 c3 c4 c5
instance (c1, c2, c3, c4, c5, c6) => CTuple6 c1 c2 c3 c4 c5 c6
instance (c1, c2, c3, c4, c5, c6, c7) => CTuple7 c1 c2 c3 c4 c5 c6 c7
instance (c1, c2, c3, c4, c5, c6, c7, c8) => CTuple8 c1 c2 c3 c4 c5 c6 c7 c8
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9)
   => CTuple9 c1 c2 c3 c4 c5 c6 c7 c8 c9
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
   => CTuple10 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
   => CTuple11 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
   => CTuple12 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
   => CTuple13 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
   => CTuple14 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
   => CTuple15 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
   => CTuple16 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17)
   => CTuple17 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17,c18)
   => CTuple18 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19)
   => CTuple19 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20)
   => CTuple20 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21)
   => CTuple21 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22)
   => CTuple22 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23)
   => CTuple23 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24)
   => CTuple24 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25)
   => CTuple25 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26)
   => CTuple26 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27)
   => CTuple27 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28)
   => CTuple28 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29)
   => CTuple29 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30)
   => CTuple30 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31)
   => CTuple31 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32)
   => CTuple32 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33)
   => CTuple33 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34)
   => CTuple34 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35)
   => CTuple35 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36)
   => CTuple36 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37)
   => CTuple37 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38)
   => CTuple38 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39)
   => CTuple39 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40)
   => CTuple40 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41)
   => CTuple41 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42)
   => CTuple42 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43)
   => CTuple43 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44)
   => CTuple44 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45)
   => CTuple45 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46)
   => CTuple46 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47)
   => CTuple47 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48)
   => CTuple48 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49)
   => CTuple49 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50)
   => CTuple50 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51)
   => CTuple51 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52)
   => CTuple52 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53)
   => CTuple53 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54)
   => CTuple54 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55)
   => CTuple55 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56)
   => CTuple56 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57)
   => CTuple57 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56 c57
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58)
   => CTuple58 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56 c57 c58
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59)
   => CTuple59 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56 c57 c58 c59
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59, c60)
   => CTuple60 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56 c57 c58 c59 c60
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59, c60, c61)
   => CTuple61 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56 c57 c58 c59 c60 c61
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59, c60, c61, c62)
   => CTuple62 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56 c57 c58 c59 c60 c61 c62
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59, c60, c61, c62, c63)
   => CTuple63 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56 c57 c58 c59 c60 c61 c62 c63
instance (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16,
       c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30,
       c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44,
       c45, c46, c47, c48, c49, c50, c51, c52, c53, c54, c55, c56, c57, c58,
       c59, c60, c61, c62, c63, c64)
   => CTuple64 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18
       c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36
       c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54
       c55 c56 c57 c58 c59 c60 c61 c62 c63 c64
