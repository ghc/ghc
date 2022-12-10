{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module T22648c where

import GHC.TypeLits (Nat)
import Data.Kind (Type)

-- 1. the (i :: Nat) parameter to avoid conflicting instances
-- 2. the (Type ->) parameter is to prevent instantiation of invisible variables

type family Infer (i :: Nat) :: Type -> forall {a}. a
type family Invis (i :: Nat) :: Type -> forall a. a
type family Vis   (i :: Nat) :: Type -> forall a -> a

type instance Vis 1 = Invis 0 -- Bad
type instance Invis 2 = Vis 0 -- Bad

type instance Vis 3 = Infer 0 -- Bad
type instance Infer 4 = Vis 0 -- Bad

type instance Invis 5 = Infer 0 -- Bad
type instance Infer 6 = Invis 0 -- Ok