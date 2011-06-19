{-# LANGUAGE TypeNaturals #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
module GHC.TypeNats
  ( -- * Basic Types
    Nat
  , NatI()
  , nat
  , natToInteger
  , checkNat

  -- * Type-Level Operations
  , type (<=) (+) (*) (^)

  -- * Natural Numbers
  , Natural(..)
  , NaturalInteger(..)
  , toNaturalInteger
  , subNatural
  ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import Data.Maybe       (Maybe(..), fromMaybe)
import GHC.Num          (Integer, Num(..))
import GHC.Real         (Integral(..), Real(..))
import GHC.Enum         (Enum(..))
import GHC.Show         (Show(..))
import GHC.Read         (Read(..))
#endif


-- | Comparsion of type-level naturals.
class (m :: Nat) <= (n :: Nat)

-- | Addition of type-level naturals.
type family (m :: Nat) + (n :: Nat) :: Nat

-- | Multiplication of type-level naturals.
type family (m :: Nat) * (n :: Nat) :: Nat

-- | Exponentiation of type-level naturals.
type family (m :: Nat) ^ (n :: Nat) :: Nat


-- | The type @Nat n@ is m \"singleton\" type containing only the value @n@.
-- (Technically, there is also m bottom element).
-- This type relates type-level naturals to run-time values.
newtype Nat (n :: Nat) = Nat Integer

-- NOTE: The instances for "NatI" are provided directly by GHC.
-- The built-in instances use the integer corresponding to the instance
-- as evidence.  This works because of the following two details about GHC:
--   * The "dictionary" for classes with a single method is the method itself,
--     so GHC simply coerces the dictionary into the value, and
--   * Newtype use the same representation as their definition types.
-- (This is a bit of a hack but it seems to work pretty well.
--  It is also possible to implement the same API in a different way.)

-- | The class 'NatI' provides a \"smart\" constructor for values
-- of type @Nat n@.  There are built-in instances for all natural numbers.
class NatI (n :: Nat) where

  -- | The only defined element of type @Nat n@.
  nat :: Nat n

-- | The integer value corresponding to a type-level natural.
natToInteger :: Nat n -> Integer
natToInteger (Nat x) = x


-- Natural numbers -------------------------------------------------------------

-- | The type of natural numbers.
data Natural        = forall n. Natural !(Nat n)


instance Show Natural where
  showsPrec p n = showsPrec p (n2i n)

instance Read Natural where
  readsPrec p s = do (x,xs) <- readsPrec p s
                     case toNaturalInteger x of
                       NonNegative n  -> [(n,xs)]
                       Negative _     -> []

instance Eq Natural where
  x == y        = n2i x == n2i y

instance Ord Natural where
  compare x y   = compare (n2i x) (n2i y)

instance Num Natural where
  x + y         = _i2n (n2i x + n2i y)
  x * y         = _i2n (n2i x * n2i y)
  x - y         = _ni2n (subNatural x y)
  abs           = id
  signum x      = _i2n (signum (n2i x))
  fromInteger x = _ni2n (toNaturalInteger x)

instance Enum Natural where
  succ x        = _i2n (1 + n2i x)
  pred x        = _ni2n (toNaturalInteger (n2i x - 1))
  toEnum x      = if x < 0 then error msg else _i2n (toEnum x)
    where msg     = "toEnum @ Natural: -ve number"
  fromEnum x    = fromEnum (n2i x)

  enumFrom x            = map _i2n (enumFrom       (n2i x))
  enumFromTo x y        = map _i2n (enumFromTo     (n2i x) (n2i y))
  enumFromThen x y
    | x <= y            = map _i2n (enumFromThen   (n2i x) (n2i y))
    | otherwise         = map _i2n (enumFromThenTo (n2i x) (n2i y) 0)
  enumFromThenTo x y z  = map _i2n (enumFromThenTo (n2i x) (n2i y) (n2i z))

instance Real Natural where
  toRational x  = toRational (n2i x)

instance Integral Natural where
  toInteger x   = n2i x
  -- Perhaps this could be made more efficient because we only
  -- deal with non -ve values?  Also, quot/div should be the same.
  quotRem x y   = case quotRem (n2i x) (n2i y) of
                    (a,b) -> (_i2n a, _i2n b)
  divMod x y    = case divMod (n2i x) (n2i y) of
                    (a,b) -> (_i2n a, _i2n b)



-- | Integers defined in terms of natural numbers.
-- This is a convenience type used to convert 'Integer' to 'Natural' values.
data NaturalInteger = Negative Natural | NonNegative Natural deriving Show

-- | Convert an integer into a natural number.
toNaturalInteger :: Integer -> NaturalInteger
toNaturalInteger x
  | x >= 0    = NonNegative (_i2n x)
  | otherwise = Negative (_i2n (negate x))

-- | Subtract two natural numbers.
subNatural :: Natural -> Natural -> NaturalInteger
subNatural x y = toNaturalInteger (n2i x - n2i y)




-- The functions bellow are derived --------------------------------------------



instance Show (Nat n) where
  showsPrec p n = showsPrec p (natToInteger n)

instance NatI n => Read (Nat n) where
  readsPrec p x       = do (x,xs) <- readsPrec p x
                           case checkNat (x ==) of
                             Just n  -> [(n,xs)]
                             Nothing -> []

-- | Returns @Just n@ if, and only if, @natToInteger n@ satisfies the
-- given predicate.
checkNat :: NatI n => (Integer -> Bool) -> Maybe (Nat n)
checkNat p = check nat
  where check y = if p (natToInteger y) then Just y else Nothing


-- PRIVATE ---------------------------------------------------------------------

-- Private. Used just for the short name.
-- Exported as 'toInteger' in class 'Integral'.
n2i :: Natural -> Integer
n2i (Natural x) = natToInteger x

-- Private.  Used only when we know that the integer is positive.
_i2n :: Integer -> Natural
_i2n x = Natural (Nat x)

-- Priavte.  Turns -ve numbers into undefined elements.
_ni2n :: NaturalInteger -> Natural
_ni2n (NonNegative x) = x
_ni2n (Negative x)    = error ("Negative natural number: -" ++ show x)


