{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module T15648 where

import Data.Kind (Type)
import Data.Type.Equality (type (~~))
import T15648a (ueqT)

data LegitEquality :: Type -> Type -> Type where
  Legit :: LegitEquality a a

data JankyEquality :: Type -> Type -> Type where
  Jank :: $ueqT a b -> JankyEquality a b

unJank :: JankyEquality a b -> $ueqT a b
unJank (Jank x) = x

legitToJank :: LegitEquality a b -> JankyEquality a b
legitToJank Legit = Jank

mkLegit :: a ~~ b => LegitEquality a b
mkLegit = Legit

ueqSym :: forall (a :: Type) (b :: Type).
          $ueqT a b -> $ueqT b a
ueqSym = unJank $ legitToJank $ mkLegit @b @a
