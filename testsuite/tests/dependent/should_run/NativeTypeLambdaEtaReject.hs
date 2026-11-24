{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures, UndecidableInstances #-}
{-# LANGUAGE Modifiers, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module NativeTypeLambdaEtaReject where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (U)
import NativeTypeLambdaEtaLib

#if defined(WRONG_CAPTURE)
bad :: (Stamp Bool) :~: (\cell -> Stamp Int cell)
bad = Refl
#elif defined(WRONG_SPINE_CAPTURE)
bad :: Stage (Stamp Int) :~: Stage (\cell -> Stamp Char cell)
bad = Refl
#elif defined(REORDERED)
bad :: (Stamp Int) :~: (\cell -> Stamp cell Int)
bad = Refl
#elif defined(CONSTANT)
bad :: (Discard Int) :~: ((\_cell -> Int) :: Type %U -> Type)
bad = Refl
#elif defined(INVERSE)
bad :: forall a b. (Discard Int a ~ Discard Int b) => a :~: b
bad = Refl
#elif defined(MATCHABLE)
bad :: Maybe :~: (\cell -> Maybe cell)
bad = Refl
#elif defined(FALLBACK)
type Same :: (Type %U -> Type) %U -> (Type %U -> Type) %U -> Bool
type family Same f g where
  Same f f = 'True
  Same _ _ = 'False

bad :: Same (Discard Int) ((\_cell -> Int) :: Type %U -> Type) :~: 'False
bad = Refl
#endif
