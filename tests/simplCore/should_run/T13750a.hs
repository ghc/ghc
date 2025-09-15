{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module T13750a where

import Data.Kind (Type)
import Unsafe.Coerce

type family AnyT :: Type where {}
type family AnyList :: [Type] where {}

newtype NP (xs :: [Type]) = NP [AnyT]

data IsNP (xs :: [Type]) where
  IsNil  :: IsNP '[]
  IsCons :: x -> NP xs -> IsNP (x ': xs)

isNP :: NP xs -> IsNP xs
isNP (NP xs) =
  if null xs
    then unsafeCoerce IsNil
    else unsafeCoerce (IsCons (head xs) (NP (tail xs)))

pattern Nil :: () => (xs ~ '[]) => NP xs
pattern Nil <- (isNP -> IsNil)
  where
    Nil = NP []

pattern (:*) :: () => (xs' ~ (x ': xs)) => x -> NP xs -> NP xs'
pattern x :* xs <- (isNP -> IsCons x xs)
  where
    x :* NP xs = NP (unsafeCoerce x : xs)
infixr 5 :*

data NS (xs :: [[Type]]) = NS !Int (NP AnyList)

data IsNS (xs :: [[Type]]) where
  IsZ :: NP x -> IsNS (x ': xs)
  IsS :: NS xs -> IsNS (x ': xs)

isNS :: NS xs -> IsNS xs
isNS (NS i x)
  | i == 0    = unsafeCoerce (IsZ (unsafeCoerce x))
  | otherwise = unsafeCoerce (IsS (NS (i - 1) x))

pattern Z :: () => (xs' ~ (x ': xs)) => NP x -> NS xs'
pattern Z x <- (isNS -> IsZ x)
  where
    Z x = NS 0 (unsafeCoerce x)

pattern S :: () => (xs' ~ (x ': xs)) => NS xs -> NS xs'
pattern S p <- (isNS -> IsS p)
