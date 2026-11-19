{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures, TypeAbstractions, TypeFamilies #-}
module NativeUnsaturatedInvisibleMatchable where
import GHC.Exts (M)

type Bad :: forall k. @M k
type family Bad @k
