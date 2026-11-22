{-# LANGUAGE DataKinds, GADTs, LinearTypes, MagicHash, RequiredTypeArguments #-}
module GadtOrderedUnboxedProvider (Payload(..), T4(..)) where

import GHC.Exts (Int#)
import GHC.Types (Multiplicity(Many))

data Payload a b c where
  Payload :: forall a. Eq a => Int# %1 -> forall b
          -> forall {hidden} c. hidden %Many -> Show hidden => Payload a b c

data T4 a where
  MkT4 :: Show a => (forall b. a -> b -> T4 a)
