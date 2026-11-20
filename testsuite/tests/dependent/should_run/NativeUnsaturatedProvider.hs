{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications, TypeFamilies, TypeOperators #-}
module NativeUnsaturatedProvider (Callback, Same, ForwardMap) where

import GHC.Exts (FUN, U, Multiplicity(Many))

type family Select (a :: Bool) (b :: Bool) :: Bool
type instance Select a 'True = a

type Callback :: Bool -> FUN @U 'Many Bool Bool
type Callback a = Select a

type Same :: FUN @U 'Many Bool Bool -> FUN @U 'Many Bool Bool -> Bool
type family Same f g where
  Same f f = 'True
  Same f g = 'False

type family Map f xs where
  Map _ '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

type family ForwardMap f xs where
  ForwardMap _ '[] = '[]
  ForwardMap f (x ': xs) = f x ': Map f xs
