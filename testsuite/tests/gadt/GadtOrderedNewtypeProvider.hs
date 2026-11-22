{-# LANGUAGE GADTs, LinearTypes, MagicHash, RequiredTypeArguments #-}
{-# LANGUAGE StandaloneKindSignatures, TypeFamilies, UnliftedNewtypes #-}
module GadtOrderedNewtypeProvider where

import Data.Kind (Type)
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

type family Id r where Id r = r

type Cast :: forall (r :: RuntimeRep). TYPE (Id r) -> Type -> TYPE (Id r)
newtype Cast a b where
  Cast :: forall (r :: RuntimeRep) (a :: TYPE (Id r)). a %1 -> forall b -> Cast @r a b
