{-# LANGUAGE LinearTypes, RequiredTypeArguments, DataKinds, TypeData #-}

module Modifier1Linear where

import GHC.Types (Multiplicity(..), Type)
import GHC.TypeLits (Nat)

f :: a %1 %01 %(1 :: Nat) -> a
f x = x
