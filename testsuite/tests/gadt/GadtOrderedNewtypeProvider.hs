{-# LANGUAGE GADTs, LinearTypes, MagicHash, RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies, UnliftedNewtypes #-}
module GadtOrderedNewtypeProvider where

import GHC.Exts (Int#, RuntimeRep(..), TYPE)

newtype N a b where
  N :: forall a. a %1 -> forall b. N a b

newtype R a b where
  R :: forall a. a %1 -> forall b -> R a b

data family Family tag a b
newtype instance Family Char a b where
  Family :: forall a. a %1 -> forall b -> Family Char a b

newtype Raw b :: TYPE IntRep where
  Raw :: Int# %1 -> forall b -> Raw b
