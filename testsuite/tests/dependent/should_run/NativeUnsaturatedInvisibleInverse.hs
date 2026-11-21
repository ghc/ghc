{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, FlexibleContexts, Modifiers, PolyKinds, RankNTypes, StandaloneKindSignatures, TypeApplications, TypeFamilies #-}
module NativeUnsaturatedInvisibleInverse where
import Data.Kind (Type)
import Data.Type.Equality
import GHC.Exts (U)
import InvisibleFamily (IndexedVisible, Required)

data Box (f :: forall k. @U k)

bad :: forall (f :: forall k. @U k) (g :: forall k. @U k).
       (f @Bool ~ g @Bool) => Box f :~: Box g
bad = Refl

badVisible :: (IndexedVisible a ~ IndexedVisible b) => a :~: b
badVisible = Refl

type RequiredCopy :: forall k -> @U k %U -> Type
type family RequiredCopy k a where
  RequiredCopy Bool 'True = Int
  RequiredCopy Type Bool = Ordering

type RequiredBox :: (forall k -> @U k %U -> Type) -> Type
data RequiredBox f

badRequired :: RequiredBox Required :~: RequiredBox RequiredCopy
badRequired = Refl
