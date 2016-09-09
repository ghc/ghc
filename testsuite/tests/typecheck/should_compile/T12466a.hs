{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module T12466a where

import GHC.TypeLits (Nat)
import Unsafe.Coerce (unsafeCoerce)

data Dict a where
  Dict :: a => Dict a

newtype a :- b = Sub (a => Dict b)

axiom :: forall a b. Dict (a ~ b)
axiom = unsafeCoerce (Dict :: Dict (a ~ a))

type Divides n m = n ~ Gcd n m
type family Gcd :: Nat -> Nat -> Nat where

dividesGcd :: forall a b c. (Divides a b, Divides a c) :- Divides a (Gcd b c)
dividesGcd = Sub axiom
