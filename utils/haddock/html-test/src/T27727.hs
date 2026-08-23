{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}

module T27727 (demonstration, f) where

import Data.Kind (Type)
import GHC.Exts (Int#, TYPE, RuntimeRep)

demonstration :: forall (f :: forall r. TYPE r -> Type) . f Int# -> f Int#
demonstration x = x

data P :: RuntimeRep -> Type

f :: (forall r. P r) -> P s
f x = x
