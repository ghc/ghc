{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Basement.Compat.Natural
    ( Natural
    , integerToNatural
    , naturalToInteger
    ) where

#if MIN_VERSION_base(4,8,0)

import Numeric.Natural
import Prelude (Integer, abs, fromInteger, toInteger)

#else

import Prelude (Show(..),Eq,Ord,Enum,Num(..),Real(..),Integral(..),Integer,error,(<), (>), otherwise, toInteger)
import Data.Bits
import Data.Typeable

newtype Natural = Natural Integer
    deriving (Eq,Ord,Enum,Typeable,Bits)

instance Show Natural where
    show (Natural i) = show i

-- re-create the buggy Num instance for Natural
instance Num Natural where
    fromInteger n
        | n < 0     = error "natural should be positive: "
        | otherwise = Natural n
    (+) (Natural a) (Natural b) = Natural (a + b)
    (-) (Natural a) (Natural b)
        | r < 0     = error "natural should be positve"
        | otherwise = Natural (a - b)
      where r = (a - b)
    (*) (Natural a) (Natural b) = Natural (a * b)
    abs n = n
    negate n = n
    signum (Natural n)
        | n > 0     = 1
        | otherwise = 0

instance Real Natural where
    toRational (Natural n) = toRational n

instance Integral Natural where
    toInteger (Natural n) = n
    divMod (Natural n) (Natural e) = let (a,b) = n `quotRem` e in (Natural a, Natural b)
    quotRem (Natural n) (Natural e) = let (a,b) = n `quotRem` e in (Natural a, Natural b)
    quot (Natural n) (Natural e) = Natural (n `quot` e)
    rem (Natural n) (Natural e) = Natural (n `rem` e)
    div = quot
    mod = rem

#endif

integerToNatural :: Integer -> Natural
integerToNatural i = fromInteger (abs i)

naturalToInteger :: Natural -> Integer
naturalToInteger n = toInteger n
