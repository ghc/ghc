{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash, UnboxedTuples, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

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

import GHC.Base
import GHC.Num
import GHC.List
import GHC.Enum
import GHC.Show
import {-# SOURCE #-} GHC.Exception( divZeroException, overflowException, ratioZeroDenomException )

#ifdef OPTIMISE_INTEGER_GCD_LCM
# if defined(MIN_VERSION_integer_gmp)
import GHC.Integer.GMP.Internals
# else
#  error unsupported OPTIMISE_INTEGER_GCD_LCM configuration
# endif
#endif

infixr 8  ^, ^^
infixl 7  /, `quot`, `rem`, `div`, `mod`
infixl 7  %

default ()              -- Double isn't available yet,
                        -- and we shouldn't be using defaults anyway

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

--------------------------------------------------------------
-- The Ratio and Rational types
--------------------------------------------------------------

-- | Rational numbers, with numerator and denominator of some 'Integral' type.
data  Ratio a = !a :% !a  deriving (Eq)

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
numerator       :: (Integral a) => Ratio a -> a

-- | Extract the denominator of the ratio in reduced form:
-- the numerator and denominator have no common factor and the denominator
-- is positive.
denominator     :: (Integral a) => Ratio a -> a


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
class  (Real a, Enum a) => Integral a  where
    -- | integer division truncated toward zero
    quot                :: a -> a -> a
    -- | integer remainder, satisfying
    --
    -- > (x `quot` y)*y + (x `rem` y) == x
    rem                 :: a -> a -> a
    -- | integer division truncated toward negative infinity
    div                 :: a -> a -> a
    -- | integer modulus, satisfying
    --
    -- > (x `div` y)*y + (x `mod` y) == x
    mod                 :: a -> a -> a
    -- | simultaneous 'quot' and 'rem'
    quotRem             :: a -> a -> (a,a)
    -- | simultaneous 'div' and 'mod'
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
class  (Num a) => Fractional a  where
    {-# MINIMAL fromRational, (recip | (/)) #-}

    -- | fractional division
    (/)                 :: a -> a -> a
    -- | reciprocal fraction
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
                                _  -> error "round default defn: Bad value"

    ceiling x           =  if r > 0 then n + 1 else n
                           where (n,r) = properFraction x

    floor x             =  if r < 0 then n - 1 else n
                           where (n,r) = properFraction x

-- These 'numeric' enumerations come straight from the Report

numericEnumFrom         :: (Fractional a) => a -> [a]
numericEnumFrom n       =  n `seq` (n : numericEnumFrom (n + 1))

numericEnumFromThen     :: (Fractional a) => a -> a -> [a]
numericEnumFromThen n m = n `seq` m `seq` (n : numericEnumFromThen m (m+m-n))

numericEnumFromTo       :: (Ord a, Fractional a) => a -> a -> [a]
numericEnumFromTo n m   = takeWhile (<= m + 1/2) (numericEnumFrom n)

numericEnumFromThenTo   :: (Ord a, Fractional a) => a -> a -> a -> [a]
numericEnumFromThenTo e1 e2 e3
    = takeWhile predicate (numericEnumFromThen e1 e2)
                                where
                                 mid = (e2 - e1) / 2
                                 predicate | e2 >= e1  = (<= e3 + mid)
                                           | otherwise = (>= e3 + mid)

--------------------------------------------------------------
-- Instances for Int
--------------------------------------------------------------

instance  Real Int  where
    toRational x        =  toInteger x :% 1

instance  Integral Int  where
    toInteger (I# i) = smallInteger i

    a `quot` b
     | b == 0                     = divZeroError
     | b == (-1) && a == minBound = overflowError -- Note [Order of tests]
                                                  -- in GHC.Int
     | otherwise                  =  a `quotInt` b

    a `rem` b
     | b == 0                     = divZeroError
       -- The quotRem CPU instruction fails for minBound `quotRem` -1,
       -- but minBound `rem` -1 is well-defined (0). We therefore
       -- special-case it.
     | b == (-1)                  = 0
     | otherwise                  =  a `remInt` b

    a `div` b
     | b == 0                     = divZeroError
     | b == (-1) && a == minBound = overflowError -- Note [Order of tests]
                                                  -- in GHC.Int
     | otherwise                  =  a `divInt` b

    a `mod` b
     | b == 0                     = divZeroError
       -- The divMod CPU instruction fails for minBound `divMod` -1,
       -- but minBound `mod` -1 is well-defined (0). We therefore
       -- special-case it.
     | b == (-1)                  = 0
     | otherwise                  =  a `modInt` b

    a `quotRem` b
     | b == 0                     = divZeroError
       -- Note [Order of tests] in GHC.Int
     | b == (-1) && a == minBound = (overflowError, 0)
     | otherwise                  =  a `quotRemInt` b

    a `divMod` b
     | b == 0                     = divZeroError
       -- Note [Order of tests] in GHC.Int
     | b == (-1) && a == minBound = (overflowError, 0)
     | otherwise                  =  a `divModInt` b

--------------------------------------------------------------
-- Instances for @Word@
--------------------------------------------------------------

instance Real Word where
    toRational x = toInteger x % 1

instance Integral Word where
    quot    (W# x#) y@(W# y#)
        | y /= 0                = W# (x# `quotWord#` y#)
        | otherwise             = divZeroError
    rem     (W# x#) y@(W# y#)
        | y /= 0                = W# (x# `remWord#` y#)
        | otherwise             = divZeroError
    div     (W# x#) y@(W# y#)
        | y /= 0                = W# (x# `quotWord#` y#)
        | otherwise             = divZeroError
    mod     (W# x#) y@(W# y#)
        | y /= 0                = W# (x# `remWord#` y#)
        | otherwise             = divZeroError
    quotRem (W# x#) y@(W# y#)
        | y /= 0                = case x# `quotRemWord#` y# of
                                  (# q, r #) ->
                                      (W# q, W# r)
        | otherwise             = divZeroError
    divMod  (W# x#) y@(W# y#)
        | y /= 0                = (W# (x# `quotWord#` y#), W# (x# `remWord#` y#))
        | otherwise             = divZeroError
    toInteger (W# x#)           = wordToInteger x#

--------------------------------------------------------------
-- Instances for Integer
--------------------------------------------------------------

instance  Real Integer  where
    toRational x        =  x :% 1

-- Note [Integer division constant folding]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Constant folding of quot, rem, div, mod, divMod and quotRem for
-- Integer arguments depends crucially on inlining. Constant folding
-- rules defined in compiler/prelude/PrelRules.lhs trigger for
-- quotInteger, remInteger and so on. So if calls to quot, rem and so on
-- were not inlined the rules would not fire. The rules would also not
-- fire if calls to quotInteger and so on were inlined, but this does not
-- happen because they are all marked with NOINLINE pragma - see documentation
-- of integer-gmp or integer-simple.

instance  Integral Integer where
    toInteger n      = n

    {-# INLINE quot #-}
    _ `quot` 0 = divZeroError
    n `quot` d = n `quotInteger` d

    {-# INLINE rem #-}
    _ `rem` 0 = divZeroError
    n `rem` d = n `remInteger` d

    {-# INLINE div #-}
    _ `div` 0 = divZeroError
    n `div` d = n `divInteger` d

    {-# INLINE mod #-}
    _ `mod` 0 = divZeroError
    n `mod` d = n `modInteger` d

    {-# INLINE divMod #-}
    _ `divMod` 0 = divZeroError
    n `divMod` d = case n `divModInteger` d of
                     (# x, y #) -> (x, y)

    {-# INLINE quotRem #-}
    _ `quotRem` 0 = divZeroError
    n `quotRem` d = case n `quotRemInteger` d of
                      (# q, r #) -> (q, r)

--------------------------------------------------------------
-- Instances for @Ratio@
--------------------------------------------------------------

instance  (Integral a)  => Ord (Ratio a)  where
    {-# SPECIALIZE instance Ord Rational #-}
    (x:%y) <= (x':%y')  =  x * y' <= x' * y
    (x:%y) <  (x':%y')  =  x * y' <  x' * y

instance  (Integral a)  => Num (Ratio a)  where
    {-# SPECIALIZE instance Num Rational #-}
    (x:%y) + (x':%y')   =  reduce (x*y' + x'*y) (y*y')
    (x:%y) - (x':%y')   =  reduce (x*y' - x'*y) (y*y')
    (x:%y) * (x':%y')   =  reduce (x * x') (y * y')
    negate (x:%y)       =  (-x) :% y
    abs (x:%y)          =  abs x :% y
    signum (x:%_)       =  signum x :% 1
    fromInteger x       =  fromInteger x :% 1

{-# RULES "fromRational/id" fromRational = id :: Rational -> Rational #-}
instance  (Integral a)  => Fractional (Ratio a)  where
    {-# SPECIALIZE instance Fractional Rational #-}
    (x:%y) / (x':%y')   =  (x*y') % (y*x')
    recip (0:%_)        = ratioZeroDenominatorError
    recip (x:%y)
        | x < 0         = negate y :% negate x
        | otherwise     = y :% x
    fromRational (x:%y) =  fromInteger x % fromInteger y

instance  (Integral a)  => Real (Ratio a)  where
    {-# SPECIALIZE instance Real Rational #-}
    toRational (x:%y)   =  toInteger x :% toInteger y

instance  (Integral a)  => RealFrac (Ratio a)  where
    {-# SPECIALIZE instance RealFrac Rational #-}
    properFraction (x:%y) = (fromInteger (toInteger q), r:%y)
                          where (q,r) = quotRem x y

instance  (Integral a, Show a)  => Show (Ratio a)  where
    {-# SPECIALIZE instance Show Rational #-}
    showsPrec p (x:%y)  =  showParen (p > ratioPrec) $
                           showsPrec ratioPrec1 x .
                           showString " % " .
                           -- H98 report has spaces round the %
                           -- but we removed them [May 04]
                           -- and added them again for consistency with
                           -- Haskell 98 [Sep 08, #1920]
                           showsPrec ratioPrec1 y

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

-- | general coercion from integral types
{-# NOINLINE [1] fromIntegral #-}
fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInteger . toInteger

{-# RULES
"fromIntegral/Int->Int" fromIntegral = id :: Int -> Int
    #-}

{-# RULES
"fromIntegral/Int->Word"  fromIntegral = \(I# x#) -> W# (int2Word# x#)
"fromIntegral/Word->Int"  fromIntegral = \(W# x#) -> I# (word2Int# x#)
"fromIntegral/Word->Word" fromIntegral = id :: Word -> Word
    #-}

-- | general coercion to fractional types
realToFrac :: (Real a, Fractional b) => a -> b
{-# NOINLINE [1] realToFrac #-}
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
{-# SPECIALISE even :: Int -> Bool #-}
{-# SPECIALISE odd  :: Int -> Bool #-}
{-# SPECIALISE even :: Integer -> Bool #-}
{-# SPECIALISE odd  :: Integer -> Bool #-}

-------------------------------------------------------
-- | raise a number to a non-negative integral power
{-# SPECIALISE [1] (^) ::
        Integer -> Integer -> Integer,
        Integer -> Int -> Integer,
        Int -> Int -> Int #-}
{-# INLINABLE [1] (^) #-}    -- See Note [Inlining (^)]
(^) :: (Num a, Integral b) => a -> b -> a
x0 ^ y0 | y0 < 0    = error "Negative exponent"
        | y0 == 0   = 1
        | otherwise = f x0 y0
    where -- f : x0 ^ y0 = x ^ y
          f x y | even y    = f (x * x) (y `quot` 2)
                | y == 1    = x
                | otherwise = g (x * x) ((y - 1) `quot` 2) x
          -- g : x0 ^ y0 = (x ^ y) * z
          g x y z | even y = g (x * x) (y `quot` 2) z
                  | y == 1 = x * z
                  | otherwise = g (x * x) ((y - 1) `quot` 2) (x * z)

-- | raise a number to an integral power
(^^)            :: (Fractional a, Integral b) => a -> b -> a
{-# INLINABLE [1] (^^) #-}         -- See Note [Inlining (^)
x ^^ n          =  if n >= 0 then x^n else recip (x^(negate n))

{- Note [Inlining (^)
   ~~~~~~~~~~~~~~~~~~~~~
   The INLINABLE pragma allows (^) to be specialised at its call sites.
   If it is called repeatedly at the same type, that can make a huge
   difference, because of those constants which can be repeatedly
   calculated.

   Currently the fromInteger calls are not floated because we get
             \d1 d2 x y -> blah
   after the gentle round of simplification. -}

{- Rules for powers with known small exponent
    see #5237
    For small exponents, (^) is inefficient compared to manually
    expanding the multiplication tree.
    Here, rules for the most common exponent types are given.
    The range of exponents for which rules are given is quite
    arbitrary and kept small to not unduly increase the number of rules.
    0 and 1 are excluded based on the assumption that nobody would
    write x^0 or x^1 in code and the cases where an exponent could
    be statically resolved to 0 or 1 are rare.

    It might be desirable to have corresponding rules also for
    exponents of other types, in particular Word, but we can't
    have those rules here (importing GHC.Word or GHC.Int would
    create a cyclic module dependency), and it's doubtful they
    would fire, since the exponents of other types tend to get
    floated out before the rule has a chance to fire.

    Also desirable would be rules for (^^), but I haven't managed
    to get those to fire.

    Note: Trying to save multiplications by sharing the square for
    exponents 4 and 5 does not save time, indeed, for Double, it is
    up to twice slower, so the rules contain flat sequences of
    multiplications.
-}

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
    | e < 0     = error "Negative exponent"
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
{-# NOINLINE [1] gcd #-}
gcd x y         =  gcd' (abs x) (abs y)
                   where gcd' a 0  =  a
                         gcd' a b  =  gcd' b (a `rem` b)

-- | @'lcm' x y@ is the smallest positive integer that both @x@ and @y@ divide.
lcm             :: (Integral a) => a -> a -> a
{-# SPECIALISE lcm :: Int -> Int -> Int #-}
{-# NOINLINE [1] lcm #-}
lcm _ 0         =  0
lcm 0 _         =  0
lcm x y         =  abs ((x `quot` (gcd x y)) * y)

#ifdef OPTIMISE_INTEGER_GCD_LCM
{-# RULES
"gcd/Int->Int->Int"             gcd = gcdInt'
"gcd/Integer->Integer->Integer" gcd = gcdInteger
"lcm/Integer->Integer->Integer" lcm = lcmInteger
 #-}

gcdInt' :: Int -> Int -> Int
gcdInt' (I# x) (I# y) = I# (gcdInt x y)

#if MIN_VERSION_integer_gmp(1,0,0)
{-# RULES
"gcd/Word->Word->Word"          gcd = gcdWord'
 #-}

gcdWord' :: Word -> Word -> Word
gcdWord' (W# x) (W# y) = W# (gcdWord x y)
#endif
#endif

integralEnumFrom :: (Integral a, Bounded a) => a -> [a]
integralEnumFrom n = map fromInteger [toInteger n .. toInteger (maxBound `asTypeOf` n)]

integralEnumFromThen :: (Integral a, Bounded a) => a -> a -> [a]
integralEnumFromThen n1 n2
  | i_n2 >= i_n1  = map fromInteger [i_n1, i_n2 .. toInteger (maxBound `asTypeOf` n1)]
  | otherwise     = map fromInteger [i_n1, i_n2 .. toInteger (minBound `asTypeOf` n1)]
  where
    i_n1 = toInteger n1
    i_n2 = toInteger n2

integralEnumFromTo :: Integral a => a -> a -> [a]
integralEnumFromTo n m = map fromInteger [toInteger n .. toInteger m]

integralEnumFromThenTo :: Integral a => a -> a -> a -> [a]
integralEnumFromThenTo n1 n2 m
  = map fromInteger [toInteger n1, toInteger n2 .. toInteger m]
