{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications, TypeFamilies #-}
module NativeUnsaturatedProvider (Callback, Same) where

import GHC.Exts (FUN, U, Multiplicity(Many))

type family Select (a :: Bool) (b :: Bool) :: Bool
type instance Select a 'True = a

type Callback :: Bool -> FUN @U 'Many Bool Bool
type Callback a = Select a

type Same :: FUN @U 'Many Bool Bool -> FUN @U 'Many Bool Bool -> Bool
type family Same f g where
  Same f f = 'True
  Same f g = 'False
