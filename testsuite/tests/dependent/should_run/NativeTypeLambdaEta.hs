{-# LANGUAGE AllowAmbiguousTypes, CPP, DataKinds, ExperimentalDependentTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs, Modifiers, ScopedTypeVariables, StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications, TypeFamilies, TypeOperators #-}
module NativeTypeLambdaEta where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (U)
import NativeTypeLambdaEtaLib

type Same :: (Type %U -> Type) %U -> (Type %U -> Type) %U -> Bool
type family Same f g where
  Same f f = 'True
  Same _ _ = 'False

update :: forall tag xs. Batch (Map (Stamp tag) xs) -> Int
update row = advance row
  where
#ifdef NAMED
    advance :: Batch (Map (Stamp tag) xs) -> Int
#else
    advance :: Batch (Map (\cell -> Stamp tag cell) xs) -> Int
#endif
    advance (Batch n) = n + 2

visible :: Int
visible = update @Int @'[Bool] (Batch 40)

#ifndef NAMED
matched :: Same (\cell -> Stamp Int cell) (Stamp Int) :~: 'True
matched = Refl

discardForward :: (Discard Int) :~: (\cell -> Discard Int cell)
discardForward = Refl

reverseMap :: forall tag xs. Proxy xs -> Map (Stamp tag) xs :~: Map (\cell -> Stamp tag cell) xs
{-# INLINE reverseMap #-}
reverseMap _ = Refl
#endif
