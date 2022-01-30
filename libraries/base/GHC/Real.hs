{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash, UnboxedTuples, BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Real
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The types 'Ratio' and 'Rational', and the classes 'Real', 'Fractional',
-- 'Integral', and 'RealFrac'.
--
-----------------------------------------------------------------------------

module GHC.Real where

#include "MachDeps.h"

import GHC.Base
import GHC.Num
import GHC.List
import GHC.Enum
import GHC.Show
import {-# SOURCE #-} GHC.Exception( divZeroException, overflowException
                                   , underflowException
                                   , ratioZeroDenomException )

import GHC.Num.BigNat (gcdInt,gcdWord)

infixr 8  ^, ^^
infixl 7  /, `quot`, `rem`, `div`, `mod`
infixl 7  %

default ()              -- Double isn't available yet,
                        -- and we shouldn't be using defaults anyway

{- Note [Allow time for type-specialisation rules to fire]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  lcm = ...
  {-# RULES "lcm/Integer->Integer->Integer" lcm = integerLcm  #-}

We want to delay inlining `lcm` until the rule (which is a form of manual
type specialisation) has had a chance to fire.  It can fire in InitialPhase,
so INLINE[2] seems sufficient.  c.f. #20709
-}

------------------------------------------------------------------------
-- Divide by zero and arithmetic overflow
------------------------------------------------------------------------

-- We put them here because they are needed relatively early
-- in the libraries before the Exception type has been defined yet.

{-# NOINLINE divZeroError #-}
divZeroError :: a
divZeroError = raise# divZeroException

{-# NOINLINE ratioZeroDenominatorError #-}
ratioZeroDenominatorError :: a
ratioZeroDenominatorError = raise# ratioZeroDenomException

{-# NOINLINE overflowError #-}
overflowError :: a
overflowError = raise# overflowException

{-# NOINLINE underflowError #-}
underflowError :: a
underflowError = raise# underflowException


--------------------------------------------------------------
-- The Ratio and Rational types
--------------------------------------------------------------

-- | Rational numbers, with numerator and denominator of some 'Integral' type.
--
-- Note that `Ratio`'s instances inherit the deficiencies from the type
-- parameter's. For example, @Ratio Natural@'s 'Num' instance has similar
-- problems to `Numeric.Natural.Natural`'s.
data  Ratio a = !a :% !a  deriving Eq -- ^ @since 2.01

-- | Arbitrary-precision rational numbers, represented as a ratio of
-- two 'Integer' values.  A rational number may be constructed using
-- the '%' operator.
type  Rational          =  Ratio Integer

ratioPrec, ratioPrec1 :: Int
ratioPrec  = 7  -- Precedence of ':%' constructor
ratioPrec1 = ratioPrec + 1

infinity, notANumber :: Rational
infinity   = 1 :% 0
notANumber = 0 :% 0

-- Use :%, not % for Inf/NaN; the latter would
-- immediately lead to a runtime error, because it normalises.

-- | Forms the ratio of two integral numbers.
{-# SPECIALISE (%) :: Integer -> Integer -> Rational #-}
(%)                     :: (Integral a) => a -> a -> Ratio a

-- | Extract the numerator of the ratio in reduced form:
-- the numerator and denominator have no common factor and the denominator
-- is positive.
numerator       :: Ratio a -> a

-- | Extract the denominator of the ratio in reduced form:
-- the numerator and denominator have no common factor and the denominator
-- is positive.
denominator     :: Ratio a -> a


-- | 'reduce' is a subsidiary function used only in this module.
-- It normalises a ratio by dividing both numerator and denominator by
-- their greatest common divisor.
reduce ::  (Integral a) => a -> a -> Ratio a
{-# SPECIALISE reduce :: Integer -> Integer -> Rational #-}
reduce _ 0              =  ratioZeroDenominatorError
reduce x y              =  (x `quot` d) :% (y `quot` d)
                           where d = gcd x y

x % y                   =  reduce (x * signum y) (abs y)

numerator   (x :% _)    =  x
denominator (_ :% y)    =  y

--------------------------------------------------------------
-- Standard numeric classes
--------------------------------------------------------------

class  (Num a, Ord a) => Real a  where
    -- | the rational equivalent of its real argument with full precision
    toRational          ::  a -> Rational

-- | Integral numbers, supporting integer division.
--
-- The Haskell Report defines no laws for 'Integral'. However, 'Integral'
-- instances are customarily expected to define a Euclidean domain and have the
-- following properties for the 'div'\/'mod' and 'quot'\/'rem' pairs, given
-- suitable Euclidean functions @f@ and @g@:
--
-- * @x@ = @y * quot x y + rem x y@ with @rem x y@ = @fromInteger 0@ or
-- @g (rem x y)@ < @g y@
-- * @x@ = @y * div x y + mod x y@ with @mod x y@ = @fromInteger 0@ or
-- @f (mod x y)@ < @f y@
--
-- An example of a suitable Euclidean function, for 'Integer'\'s instance, is
-- 'abs'.
class  (Real a, Enum a) => Integral a  where
    -- | integer division truncated toward zero
    --
    -- WARNING: This function is partial (because it throws when 0 is passed as
    -- the divisor) for all the integer types in @base@.
    quot                :: a -> a -> a
    -- | integer remainder, satisfying
    --
    -- > (x `quot` y)*y + (x `rem` y) == x
    --
    -- WARNING: This function is partial (because it throws when 0 is passed as
    -- the divisor) for all the integer types in @base@.
    rem                 :: a -> a -> a
    -- | integer division truncated toward negative infinity
    --
    -- WARNING: This function is partial (because it throws when 0 is passed as
    -- the divisor) for all the integer types in @base@.
    div                 :: a -> a -> a
    -- | integer modulus, satisfying
    --
    -- > (x `div` y)*y + (x `mod` y) == x
    --
    -- WARNING: This function is partial (because it throws when 0 is passed as
    -- the divisor) for all the integer types in @base@.
    mod                 :: a -> a -> a
    -- | simultaneous 'quot' and 'rem'
    --
    -- WARNING: This function is partial (because it throws when 0 is passed as
    -- the divisor) for all the integer types in @base@.
    quotRem             :: a -> a -> (a,a)
    -- | simultaneous 'div' and 'mod'
    --
    -- WARNING: This function is partial (because it throws when 0 is passed as
    -- the divisor) for all the integer types in @base@.
    divMod              :: a -> a -> (a,a)
    -- | conversion to 'Integer'
    toInteger           :: a -> Integer

    {-# INLINE quot #-}
    {-# INLINE rem #-}
    {-# INLINE div #-}
    {-# INLINE mod #-}
    n `quot` d          =  q  where (q,_) = quotRem n d
    n `rem` d           =  r  where (_,r) = quotRem n d
    n `div` d           =  q  where (q,_) = divMod n d
    n `mod` d           =  r  where (_,r) = divMod n d

    divMod n d          =  if signum r == negate (signum d) then (q-1, r+d) else qr
                           where qr@(q,r) = quotRem n d

-- | Fractional numbers, supporting real division.
--
-- The Haskell Report defines no laws for 'Fractional'. However, @('+')@ and
-- @('*')@ are customarily expected to define a division ring and have the
-- following properties:
--
-- [__'recip' gives the multiplicative inverse__]:
-- @x * recip x@ = @recip x * x@ = @fromInteger 1@
--
-- Note that it /isn't/ customarily expected that a type instance of
-- 'Fractional' implement a field. However, all instances in @base@ do.
class  (Num a) => Fractional a  where
    {-# MINIMAL fromRational, (recip | (/)) #-}

    -- | Fractional division.
    (/)                 :: a -> a -> a
    -- | Reciprocal fraction.
    recip               :: a -> a
    -- | Conversion from a 'Rational' (that is @'Ratio' 'Integer'@).
    -- A floating literal stands for an application of 'fromRational'
    -- to a value of type 'Rational', so such literals have type
    -- @('Fractional' a) => a@.
    fromRational        :: Rational -> a

    {-# INLINE recip #-}
    {-# INLINE (/) #-}
    recip x             =  1 / x
    x / y               = x * recip y

-- | Extracting components of fractions.
class  (Real a, Fractional a) => RealFrac a  where
    -- | The function 'properFraction' takes a real fractional number @x@
    -- and returns a pair @(n,f)@ such that @x = n+f@, and:
    --
    -- * @n@ is an integral number with the same sign as @x@; and
    --
    -- * @f@ is a fraction with the same type and sign as @x@,
    --   and with absolute value less than @1@.
    --
    -- The default definitions of the 'ceiling', 'floor', 'truncate'
    -- and 'round' functions are in terms of 'properFraction'.
    properFraction      :: (Integral b) => a -> (b,a)
    -- | @'truncate' x@ returns the integer nearest @x@ between zero and @x@
    truncate            :: (Integral b) => a -> b
    -- | @'round' x@ returns the nearest integer to @x@;
    --   the even integer if @x@ is equidistant between two integers
    round               :: (Integral b) => a -> b
    -- | @'ceiling' x@ returns the least integer not less than @x@
    ceiling             :: (Integral b) => a -> b
    -- | @'floor' x@ returns the greatest integer not greater than @x@
    floor               :: (Integral b) => a -> b

    {-# INLINE truncate #-}
    truncate x          =  m  where (m,_) = properFraction x

    round x             =  let (n,r) = properFraction x
                               m     = if r < 0 then n - 1 else n + 1
                           in case signum (abs r - 0.5) of
                                -1 -> n
                                0  -> if even n then n else m
                                1  -> m
                                _  -> errorWithoutStackTrace "round default defn: Bad value"

    ceiling x           =  if r > 0 then n + 1 else n
                           where (n,r) = properFraction x

    floor x             =  if r < 0 then n - 1 else n
                           where (n,r) = properFraction x

-- These 'numeric' enumerations come straight from the Report

numericEnumFrom         :: (Fractional a) => a -> [a]
numericEnumFrom n       = go 0
  where
    -- See Note [Numeric Stability of Enumerating Floating Numbers]
    go !k = let !n' = n + k
             in n' : go (k + 1)

numericEnumFromThen     :: (Fractional a) => a -> a -> [a]
numericEnumFromThen n m = go 0
  where
    step = m - n
    -- See Note [Numeric Stability of Enumerating Floating Numbers]
    go !k = let !n' = n + k * step
             in n' : go (k + 1)

numericEnumFromTo       :: (Ord a, Fractional a) => a -> a -> [a]
numericEnumFromTo n m   = takeWhile (<= m + 1/2) (numericEnumFrom n)

numericEnumFromThenTo   :: (Ord a, Fractional a) => a -> a -> a -> [a]
numericEnumFromThenTo e1 e2 e3
    = takeWhile predicate (numericEnumFromThen e1 e2)
                                where
                                 mid = (e2 - e1) / 2
                                 predicate | e2 >= e1  = (<= e3 + mid)
                                           | otherwise = (>= e3 + mid)

{- Note [Numeric Stability of Enumerating Floating Numbers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When enumerate floating numbers, we could add the increment to the last number
at every run (as what we did previously):

    numericEnumFrom n =  n `seq` (n : numericEnumFrom (n + 1))

This approach is concise and really fast, only needs an addition operation.
However when a floating number is large enough, for `n`, `n` and `n+1` will
have the same binary representation. For example (all number has type
`Double`):

    9007199254740990                 is: 0x433ffffffffffffe
    9007199254740990 + 1             is: 0x433fffffffffffff
    (9007199254740990 + 1) + 1       is: 0x4340000000000000
    ((9007199254740990 + 1) + 1) + 1 is: 0x4340000000000000

When we evaluate ([9007199254740990..9007199254740991] :: Double), we would
never reach the condition in `numericEnumFromTo`

    9007199254740990 + 1 + 1 + ... > 9007199254740991 + 1/2

We would fall into infinite loop (as reported in #15081).

To remedy the situation, we record the number of `1` that needed to be added
to the start number, rather than increasing `1` at every time. This approach
can improvement the numeric stability greatly at the cost of a multiplication.

Furthermore, we use the type of the enumerated number, `Fractional a => a`,
as the type of multiplier. In rare situations, the multiplier could be very
large and will lead to the enumeration to infinite loop, too, which should
be very rare. Consider the following example:

    [1..9007199254740994]

We could fix that by using an Integer as multiplier but we don't do that.
The benchmark on T7954.hs shows that this approach leads to significant
degeneration on performance (33% increase allocation and 300% increase on
elapsed time).

See #15081 and Phab:D4650 for the related discussion about this problem.
-}

--------------------------------------------------------------
-- Instances for Int
--------------------------------------------------------------

-- | @since 2.0.1
instance  Real Int  where
    toRational x        =  toInteger x :% 1

-- | @since 2.0.1
instance Integral Int where
    toInteger (I# i) = IS i

    {-# INLINE quot #-} -- see Note [INLINE division wrappers] in GHC.Base
    a `quot` b
     | b == 0                     = divZeroError
     | b == (-1) && a == minBound = overflowError -- Note [Order of tests]
                                                  -- in GHC.Int
     | otherwise                  =  a `quotInt` b

    {-# INLINE rem #-} -- see Note [INLINE division wrappers] in GHC.Base
    !a `rem` b -- See Note [Special case of mod and rem is lazy]
     | b == 0                     = divZeroError
     | b == (-1)                  = 0
     | otherwise                  =  a `remInt` b

    {-# INLINE div #-} -- see Note [INLINE division wrappers] in GHC.Base
    a `div` b
     | b == 0                     = divZeroError
     | b == (-1) && a == minBound = overflowError -- Note [Order of tests]
                                                  -- in GHC.Int
     | otherwise                  =  a `divInt` b

    {-# INLINE mod #-} -- see Note [INLINE division wrappers] in GHC.Base
    !a `mod` b -- See Note [Special case of mod and rem is lazy]
     | b == 0                     = divZeroError
     | b == (-1)                  = 0
     | otherwise                  =  a `modInt` b

    {-# INLINE quotRem #-} -- see Note [INLINE division wrappers] in GHC.Base
    a `quotRem` b
     | b == 0                     = divZeroError
       -- Note [Order of tests] in GHC.Int
     | b == (-1) && a == minBound = (overflowError, 0)
     | otherwise                  =  a `quotRemInt` b

    {-# INLINE divMod #-} -- see Note [INLINE division wrappers] in GHC.Base
    a `divMod` b
     | b == 0                     = divZeroError
       -- Note [Order of tests] in GHC.Int
     | b == (-1) && a == minBound = (overflowError, 0)
     | otherwise                  =  a `divModInt` b

{- Note [Special case of mod and rem is lazy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The `quotRem`/`divMod` CPU instruction fails for minBound `quotRem` -1, but
minBound `rem` -1 is well-defined (0). We therefore special-case for `b == -1`,
but not for `a == minBound` because of Note [Order of tests] in GHC.Int. But
now we have to make sure the function stays strict in a, to guarantee unboxing.
Hence the bang on a, see #18187.
-}

--------------------------------------------------------------
-- Instances for @Word@
--------------------------------------------------------------

-- | @since 2.01
instance Real Word where
    toRational x = toInteger x % 1

-- | @since 2.01
instance Integral Word where
    -- see Note [INLINE division wrappers] in GHC.Base
    {-# INLINE quot    #-}
    {-# INLINE rem     #-}
    {-# INLINE quotRem #-}
    {-# INLINE div     #-}
    {-# INLINE mod     #-}
    {-# INLINE divMod  #-}

    quot    (W# x#) y@(W# y#)
        | y /= 0                = W# (x# `quotWord#` y#)
        | otherwise             = divZeroError

    rem     (W# x#) y@(W# y#)
        | y /= 0                = W# (x# `remWord#` y#)
        | otherwise             = divZeroError

    quotRem (W# x#) y@(W# y#)
        | y /= 0                = case x# `quotRemWord#` y# of
                                  (# q, r #) ->
                                      (W# q, W# r)
        | otherwise             = divZeroError

    div    x y = quot x y
    mod    x y = rem x y
    divMod x y = quotRem x y

    toInteger (W# x#)           = integerFromWord# x#

--------------------------------------------------------------
-- Instances for Integer
--------------------------------------------------------------

-- | @since 2.0.1
instance  Real Integer  where
    toRational x        =  x :% 1

-- | @since 4.8.0.0
instance Real Natural where
    toRational n = integerFromNatural n :% 1

-- Note [Integer division constant folding]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Constant folding of quot, rem, div, mod, divMod and quotRem for Integer
-- arguments depends crucially on inlining. Constant folding rules defined in
-- GHC.Core.Opt.ConstantFold trigger for integerQuot, integerRem and so on.
-- So if calls to quot, rem and so on were not inlined the rules would not fire.
--
-- The rules would also not fire if calls to integerQuot and so on were inlined,
-- but this does not happen because they are all marked with NOINLINE pragma.


-- | @since 2.0.1
instance Integral Integer where
    -- see Note [INLINE division wrappers] in GHC.Base
    {-# INLINE quot    #-}
    {-# INLINE rem     #-}
    {-# INLINE quotRem #-}
    {-# INLINE div     #-}
    {-# INLINE mod     #-}
    {-# INLINE divMod  #-}

    toInteger n      = n

    !_ `quot` 0 = divZeroError
    n  `quot` d = n `integerQuot` d

    !_ `rem` 0 = divZeroError
    n  `rem` d = n `integerRem` d

    !_ `div` 0 = divZeroError
    n  `div` d = n `integerDiv` d

    !_ `mod` 0 = divZeroError
    n  `mod` d = n `integerMod` d

    !_ `divMod` 0 = divZeroError
    n  `divMod` d = n `integerDivMod` d

    !_ `quotRem` 0 = divZeroError
    n  `quotRem` d = n `integerQuotRem` d

-- | @since 4.8.0.0
instance Integral Natural where
    -- see Note [INLINE division wrappers] in GHC.Base
    {-# INLINE quot    #-}
    {-# INLINE rem     #-}
    {-# INLINE quotRem #-}
    {-# INLINE div     #-}
    {-# INLINE mod     #-}
    {-# INLINE divMod  #-}

    toInteger x = integerFromNatural x

    !_ `quot` 0 = divZeroError
    n  `quot` d = n `naturalQuot` d

    !_ `rem` 0 = divZeroError
    n  `rem` d = n `naturalRem` d

    !_ `quotRem` 0 = divZeroError
    n  `quotRem` d = n `naturalQuotRem` d

    div    x y = quot x y
    mod    x y = rem x y
    divMod x y = quotRem x y

--------------------------------------------------------------
-- Instances for @Ratio@
--------------------------------------------------------------

-- | @since 2.0.1
instance  (Integral a)  => Ord (Ratio a)  where
    {-# SPECIALIZE instance Ord Rational #-}
    (x:%y) <= (x':%y')  =  x * y' <= x' * y
    (x:%y) <  (x':%y')  =  x * y' <  x' * y

-- | @since 2.0.1
instance  (Integral a)  => Num (Ratio a)  where
    {-# SPECIALIZE instance Num Rational #-}
    (x:%y) + (x':%y')   =  reduce (x*y' + x'*y) (y*y')
    (x:%y) - (x':%y')   =  reduce (x*y' - x'*y) (y*y')
    (x:%y) * (x':%y')   =  reduce (x * x') (y * y')
    negate (x:%y)       =  (-x) :% y
    abs (x:%y)          =  abs x :% y
    signum (x:%_)       =  signum x :% 1
    fromInteger x       =  fromInteger x :% 1

-- | @since 2.0.1
{-# RULES "fromRational/id" fromRational = id :: Rational -> Rational #-}
instance  (Integral a)  => Fractional (Ratio a)  where
    {-# SPECIALIZE instance Fractional Rational #-}
    (x:%y) / (x':%y')   =  (x*y') % (y*x')
    recip (0:%_)        = ratioZeroDenominatorError
    recip (x:%y)
        | x < 0         = negate y :% negate x
        | otherwise     = y :% x
    fromRational (x:%y) =  fromInteger x % fromInteger y

-- | @since 2.0.1
instance  (Integral a)  => Real (Ratio a)  where
    {-# SPECIALIZE instance Real Rational #-}
    toRational (x:%y)   =  toInteger x :% toInteger y

-- | @since 2.0.1
instance  (Integral a)  => RealFrac (Ratio a)  where
    {-# SPECIALIZE instance RealFrac Rational #-}
    properFraction (x:%y) = (fromInteger (toInteger q), r:%y)
                          where (q,r) = quotRem x y
    round r =
      let
        (n, f) = properFraction r
        x = if r < 0 then -1 else 1
      in
        case (compare (abs f) 0.5, odd n) of
          (LT, _) -> n
          (EQ, False) -> n
          (EQ, True) -> n + x
          (GT, _) -> n + x

-- | @since 2.0.1
instance  (Show a)  => Show (Ratio a)  where
    {-# SPECIALIZE instance Show Rational #-}
    showsPrec p (x:%y)  =  showParen (p > ratioPrec) $
                           showsPrec ratioPrec1 x .
                           showString " % " .
                           -- H98 report has spaces round the %
                           -- but we removed them [May 04]
                           -- and added them again for consistency with
                           -- Haskell 98 [Sep 08, #1920]
                           showsPrec ratioPrec1 y

-- | @since 2.0.1
instance  (Integral a)  => Enum (Ratio a)  where
    {-# SPECIALIZE instance Enum Rational #-}
    succ x              =  x + 1
    pred x              =  x - 1

    toEnum n            =  fromIntegral n :% 1
    fromEnum            =  fromInteger . truncate

    enumFrom            =  numericEnumFrom
    enumFromThen        =  numericEnumFromThen
    enumFromTo          =  numericEnumFromTo
    enumFromThenTo      =  numericEnumFromThenTo

--------------------------------------------------------------
-- Coercions
--------------------------------------------------------------

-- | General coercion from 'Integral' types.
--
-- WARNING: This function performs silent truncation if the result type is not
-- at least as big as the argument's type.
{-# INLINE fromIntegral #-}
  -- Inlined to allow built-in rules to match.
  -- See Note [Optimising conversions between numeric types]
  -- in GHC.Core.Opt.ConstantFold
fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInteger . toInteger

-- | General coercion to 'Fractional' types.
--
-- WARNING: This function goes through the 'Rational' type, which does not have values for 'NaN' for example.
-- This means it does not round-trip.
--
-- For 'Double' it also behaves differently with or without -O0:
--
-- > Prelude> realToFrac nan -- With -O0
-- > -Infinity
-- > Prelude> realToFrac nan
-- > NaN
realToFrac :: (Real a, Fractional b) => a -> b
{-# NOINLINE [1] realToFrac #-}
-- See Note [Allow time for type-specialisation rules to fire]
-- These rule actually appear in other modules, e.g. GHC.Float
realToFrac = fromRational . toRational

--------------------------------------------------------------
-- Overloaded numeric functions
--------------------------------------------------------------

-- | Converts a possibly-negative 'Real' value to a string.
showSigned :: (Real a)
  => (a -> ShowS)       -- ^ a function that can show unsigned values
  -> Int                -- ^ the precedence of the enclosing context
  -> a                  -- ^ the value to show
  -> ShowS
showSigned showPos p x
   | x < 0     = showParen (p > 6) (showChar '-' . showPos (-x))
   | otherwise = showPos x

even, odd       :: (Integral a) => a -> Bool
even n          =  n `rem` 2 == 0
odd             =  not . even
{-# INLINABLE even #-}
{-# INLINABLE odd  #-}

-------------------------------------------------------
-- | raise a number to a non-negative integral power
{-# SPECIALISE [1] (^) ::
        Integer -> Integer -> Integer,
        Integer -> Int -> Integer,
        Int -> Int -> Int #-}
{-# INLINABLE [1] (^) #-}    -- See Note [Inlining (^)]
(^) :: (Num a, Integral b) => a -> b -> a
x0 ^ y0 | y0 < 0    = errorWithoutStackTrace "Negative exponent"
        | y0 == 0   = 1
        | otherwise = f x0 y0
    where -- f : x0 ^ y0 = x ^ y
          f x y | even y    = f (x * x) (y `quot` 2)
                | y == 1    = x
                | otherwise = g (x * x) (y `quot` 2) x         -- See Note [Half of y - 1]
          -- g : x0 ^ y0 = (x ^ y) * z
          g x y z | even y = g (x * x) (y `quot` 2) z
                  | y == 1 = x * z
                  | otherwise = g (x * x) (y `quot` 2) (x * z) -- See Note [Half of y - 1]

-- | raise a number to an integral power
(^^)            :: (Fractional a, Integral b) => a -> b -> a
{-# INLINABLE [1] (^^) #-}         -- See Note [Inlining (^)
x ^^ n          =  if n >= 0 then x^n else recip (x^(negate n))

{- Note [Half of y - 1]
~~~~~~~~~~~~~~~~~~~~~~~~
Since y is guaranteed to be odd and positive here,
half of y - 1 can be computed as y `quot` 2, optimising subtraction away.

Note [Inlining (^)
~~~~~~~~~~~~~~~~~~
The INLINABLE pragma allows (^) to be specialised at its call sites.
If it is called repeatedly at the same type, that can make a huge
difference, because of those constants which can be repeatedly
calculated.

Currently the fromInteger calls are not floated because we get
          \d1 d2 x y -> blah
after the gentle round of simplification.

Note [Powers with small exponent]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For small exponents, (^) is inefficient compared to manually
expanding the multiplication tree (see #5237).

Here, rules for the most common exponent types are given.
The range of exponents for which rules are given is quite
arbitrary and kept small to not unduly increase the number of rules.
0 and 1 are excluded based on the assumption that nobody would
write x^0 or x^1 in code and the cases where an exponent could
be statically resolved to 0 or 1 are rare.

It might be desirable to have corresponding rules also for
exponents of other types (e. g., Word), but it's doubtful they
would fire, since the exponents of other types tend to get
floated out before the rule has a chance to fire.

Also desirable would be rules for (^^), but I haven't managed
to get those to fire.

Note: Trying to save multiplications by sharing the square for
exponents 4 and 5 does not save time, indeed, for Double, it is
up to twice slower, so the rules contain flat sequences of
multiplications.
-}

-- See Note [Powers with small exponent]
{-# RULES
"^2/Int"        forall x. x ^ (2 :: Int) = let u = x in u*u
"^3/Int"        forall x. x ^ (3 :: Int) = let u = x in u*u*u
"^4/Int"        forall x. x ^ (4 :: Int) = let u = x in u*u*u*u
"^5/Int"        forall x. x ^ (5 :: Int) = let u = x in u*u*u*u*u
"^2/Integer"    forall x. x ^ (2 :: Integer) = let u = x in u*u
"^3/Integer"    forall x. x ^ (3 :: Integer) = let u = x in u*u*u
"^4/Integer"    forall x. x ^ (4 :: Integer) = let u = x in u*u*u*u
"^5/Integer"    forall x. x ^ (5 :: Integer) = let u = x in u*u*u*u*u
  #-}

-------------------------------------------------------
-- Special power functions for Rational
--
-- see #4337
--
-- Rationale:
-- For a legitimate Rational (n :% d), the numerator and denominator are
-- coprime, i.e. they have no common prime factor.
-- Therefore all powers (n ^ a) and (d ^ b) are also coprime, so it is
-- not necessary to compute the greatest common divisor, which would be
-- done in the default implementation at each multiplication step.
-- Since exponentiation quickly leads to very large numbers and
-- calculation of gcds is generally very slow for large numbers,
-- avoiding the gcd leads to an order of magnitude speedup relatively
-- soon (and an asymptotic improvement overall).
--
-- Note:
-- We cannot use these functions for general Ratio a because that would
-- change results in a multitude of cases.
-- The cause is that if a and b are coprime, their remainders by any
-- positive modulus generally aren't, so in the default implementation
-- reduction occurs.
--
-- Example:
-- (17 % 3) ^ 3 :: Ratio Word8
-- Default:
-- (17 % 3) ^ 3 = ((17 % 3) ^ 2) * (17 % 3)
--              = ((289 `mod` 256) % 9) * (17 % 3)
--              = (33 % 9) * (17 % 3)
--              = (11 % 3) * (17 % 3)
--              = (187 % 9)
-- But:
-- ((17^3) `mod` 256) % (3^3)   = (4913 `mod` 256) % 27
--                              = 49 % 27
--
-- TODO:
-- Find out whether special-casing for numerator, denominator or
-- exponent = 1 (or -1, where that may apply) gains something.

-- Special version of (^) for Rational base
{-# RULES "(^)/Rational"    (^) = (^%^) #-}
(^%^)           :: Integral a => Rational -> a -> Rational
(n :% d) ^%^ e
    | e < 0     = errorWithoutStackTrace "Negative exponent"
    | e == 0    = 1 :% 1
    | otherwise = (n ^ e) :% (d ^ e)

-- Special version of (^^) for Rational base
{-# RULES "(^^)/Rational"   (^^) = (^^%^^) #-}
(^^%^^)         :: Integral a => Rational -> a -> Rational
(n :% d) ^^%^^ e
    | e > 0     = (n ^ e) :% (d ^ e)
    | e == 0    = 1 :% 1
    | n > 0     = (d ^ (negate e)) :% (n ^ (negate e))
    | n == 0    = ratioZeroDenominatorError
    | otherwise = let nn = d ^ (negate e)
                      dd = (negate n) ^ (negate e)
                  in if even e then (nn :% dd) else (negate nn :% dd)

-------------------------------------------------------
-- | @'gcd' x y@ is the non-negative factor of both @x@ and @y@ of which
-- every common factor of @x@ and @y@ is also a factor; for example
-- @'gcd' 4 2 = 2@, @'gcd' (-4) 6 = 2@, @'gcd' 0 4@ = @4@. @'gcd' 0 0@ = @0@.
-- (That is, the common divisor that is \"greatest\" in the divisibility
-- preordering.)
--
-- Note: Since for signed fixed-width integer types, @'abs' 'minBound' < 0@,
-- the result may be negative if one of the arguments is @'minBound'@ (and
-- necessarily is if the other is @0@ or @'minBound'@) for such types.
gcd             :: (Integral a) => a -> a -> a
{-# SPECIALISE gcd :: Int -> Int -> Int #-}
{-# SPECIALISE gcd :: Word -> Word -> Word #-}
{-# NOINLINE [2] gcd #-} -- See Note [Allow time for type-specialisation rules to fire]
gcd x y         =  gcd' (abs x) (abs y)
                   where gcd' a 0  =  a
                         gcd' a b  =  gcd' b (a `rem` b)

-- | @'lcm' x y@ is the smallest positive integer that both @x@ and @y@ divide.
lcm             :: (Integral a) => a -> a -> a
{-# SPECIALISE lcm :: Int -> Int -> Int #-}
{-# SPECIALISE lcm :: Word -> Word -> Word #-}
{-# NOINLINE [2] lcm #-} -- See Note [Allow time for type-specialisation rules to fire]
lcm _ 0         =  0
lcm 0 _         =  0
lcm x y         =  abs ((x `quot` (gcd x y)) * y)

{-# RULES
"gcd/Integer->Integer->Integer" gcd = integerGcd
"lcm/Integer->Integer->Integer" lcm = integerLcm
"gcd/Natural->Natural->Natural" gcd = naturalGcd
"lcm/Natural->Natural->Natural" lcm = naturalLcm
 #-}

{-# RULES
"gcd/Int->Int->Int"             gcd = gcdInt
"gcd/Word->Word->Word"          gcd = gcdWord
 #-}

-- See Note [Stable Unfolding for list producers] in GHC.Enum
{-# INLINABLE integralEnumFrom #-}
integralEnumFrom :: (Integral a, Bounded a) => a -> [a]
integralEnumFrom n = map fromInteger [toInteger n .. toInteger (maxBound `asTypeOf` n)]

-- See Note [Stable Unfolding for list producers] in GHC.Enum
{-# INLINABLE integralEnumFromThen #-}
integralEnumFromThen :: (Integral a, Bounded a) => a -> a -> [a]
integralEnumFromThen n1 n2
  | i_n2 >= i_n1  = map fromInteger [i_n1, i_n2 .. toInteger (maxBound `asTypeOf` n1)]
  | otherwise     = map fromInteger [i_n1, i_n2 .. toInteger (minBound `asTypeOf` n1)]
  where
    i_n1 = toInteger n1
    i_n2 = toInteger n2

-- See Note [Stable Unfolding for list producers] in GHC.Enum
{-# INLINABLE integralEnumFromTo #-}
integralEnumFromTo :: Integral a => a -> a -> [a]
integralEnumFromTo n m = map fromInteger [toInteger n .. toInteger m]

-- See Note [Stable Unfolding for list producers] in GHC.Enum
{-# INLINABLE integralEnumFromThenTo #-}
integralEnumFromThenTo :: Integral a => a -> a -> a -> [a]
integralEnumFromThenTo n1 n2 m
  = map fromInteger [toInteger n1, toInteger n2 .. toInteger m]

-- mkRational related code

data FractionalExponentBase
  = Base2
  | Base10
  deriving (Show)

mkRationalBase2 :: Rational -> Integer -> Rational
mkRationalBase2 r e = mkRationalWithExponentBase r e Base2

mkRationalBase10 :: Rational -> Integer -> Rational
mkRationalBase10 r e = mkRationalWithExponentBase r e Base10

mkRationalWithExponentBase :: Rational -> Integer
                           -> FractionalExponentBase -> Rational
mkRationalWithExponentBase r e feb = r * (eb ^^ e)
  -- See Note [fractional exponent bases] for why only these bases.
  where eb = case feb of Base2 -> 2 ; Base10 -> 10
