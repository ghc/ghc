{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module T13674 where

import Data.Proxy
import GHC.Exts (Constraint)
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

data Dict :: Constraint -> * where
  Dict :: a => Dict a

infixr 9 :-
newtype a :- b = Sub (a => Dict b)

-- | Given that @a :- b@, derive something that needs a context @b@, using the context @a@
(\\) :: a => (b => r) -> (a :- b) -> r
r \\ Sub Dict = r

newtype Magic n = Magic (KnownNat n => Dict (KnownNat n))

magic :: forall n m o. (Integer -> Integer -> Integer) -> (KnownNat n, KnownNat m) :- KnownNat o
magic f = Sub $ unsafeCoerce (Magic Dict) (natVal (Proxy :: Proxy n) `f` natVal (Proxy :: Proxy m))

type family Lcm :: Nat -> Nat -> Nat where

axiom :: forall a b. Dict (a ~ b)
axiom = unsafeCoerce (Dict :: Dict (a ~ a))

lcmNat :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (Lcm n m)
lcmNat = magic lcm

lcmIsIdempotent :: forall n. Dict (n ~ Lcm n n)
lcmIsIdempotent = axiom

newtype GF (n :: Nat) = GF Integer

x :: GF 5
x = GF 3

y :: GF 5
y = GF 4

foo :: (KnownNat m, KnownNat n) => GF m -> GF n -> GF (Lcm m n)
foo m@(GF x) n@(GF y) = GF $ (x*y) `mod` (lcm (natVal m) (natVal n))

bar :: (KnownNat m)  => GF m -> GF m -> GF m
bar (x :: GF m) y = foo x y - foo y x \\ lcmNat @m @m \\ Sub @() (lcmIsIdempotent @m)
