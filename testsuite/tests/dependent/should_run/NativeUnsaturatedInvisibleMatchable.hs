{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, PolyKinds, RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures, TypeAbstractions, TypeFamilies #-}
module NativeUnsaturatedInvisibleMatchable where
import Data.Kind (Type)
import GHC.Exts (M, U)
import InvisibleFamily (PickVisible, ReadAt, Required)

type Bad :: forall k. @M k
type family Bad @k

type ExpectsM :: forall r. (forall (k :: Type). @M r) -> Type
data ExpectsM f

type BadVisible = ExpectsM (PickVisible 'True)

type BadSuffix = ExpectsM (ReadAt 'True)

type ExpectsRequiredM :: (forall k -> @M k %U -> Type) -> Type
data ExpectsRequiredM f

type BadRequired = ExpectsRequiredM Required
