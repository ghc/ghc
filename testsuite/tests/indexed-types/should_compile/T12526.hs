{-# LANGUAGE TypeFamilies, MonoLocalBinds #-}
module T12526 where

import Data.Kind (Type)

type family P (s :: Type -> Type) :: Type -> Type -> Type
type instance P Signal = Causal

type family S (p :: Type -> Type -> Type) :: Type -> Type
type instance S Causal = Signal

class (P (S p) ~ p) => CP p
instance CP Causal

data Signal a = Signal
data Causal a b = Causal

shapeModOsci :: CP p => p Float Float
shapeModOsci = undefined

f :: Causal Float Float -> Bool
f = undefined

-- This fails
ping :: Bool
ping = let osci = shapeModOsci in  f osci


-- This works
-- ping :: Bool
-- ping = f shapeModOsci


{-

    osci :: p Float Float
    [W] CP p, [D] P (S p) ~ p
-->
    [W] CP p, [D] P fuv1 ~ fuv2, S p ~ fuv1, fuv2 ~ p
-->
    p := fuv2
    [W] CP fuv2, [D] P fuv1 ~ fuv2, S fuv2 ~ fuv1

-}

-- P (S p) ~ p
-- p Float Float ~ Causal Float Float


{-
  P (S p) ~ p
  p Float Float ~ Causal Float Float

--->
  S p ~ fuv1    (FunEq)
  P fuv1 ~ fuv2 (FunEq)
  fuv2 ~ p
  p F F ~ Causal F F

--->
  p := fuv2

  fuv2 ~ Causal
  S fuv2 ~ fuv1 (FunEq)
  P fuv1 ~ fuv2 (FunEq)

---> unflatten
  fuv1 := S fuv2
  fuv2 := Causal

-}
