{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies, TypeOperators #-}

module NativeRuleProofProvider (Image, ImageAlias, imageProof, other) where

import Data.Type.Equality ((:~:)(Refl))

type family Image :: Maybe Bool where
    Image = Prelude.fmap Prelude.not ('Just 'True)

type ImageAlias = Prelude.fmap Prelude.not ('Just 'True)

{-# INLINE imageProof #-}
imageProof :: Image :~: 'Just 'False
imageProof = Refl

other :: Int
other = 42
