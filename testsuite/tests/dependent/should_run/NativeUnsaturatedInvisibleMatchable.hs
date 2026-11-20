{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures, TypeAbstractions, TypeFamilies #-}
module NativeUnsaturatedInvisibleMatchable where
import Data.Kind (Type)
import GHC.Exts (M)
import InvisibleFamily (PickVisible)

type Bad :: forall k. @M k
type family Bad @k

type ExpectsM :: forall r. (forall (k :: Type). @M r) -> Type
data ExpectsM f

type BadVisible = ExpectsM (PickVisible 'True)
