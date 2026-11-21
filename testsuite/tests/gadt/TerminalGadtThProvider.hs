{-# LANGUAGE DataKinds, GADTs, LinearTypes, RequiredTypeArguments #-}
module TerminalGadtThProvider (Original(..)) where

import Data.Kind (Type)
import GHC.Exts (Multiplicity(Many))

data Original (a :: Type) (b :: Type) (c :: Type) where
  After :: forall a. Eq a => (Show a => !a %Many -> forall b c -> Original a b c)
