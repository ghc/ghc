{-# LANGUAGE ExplicitNamespaces, GADTs, LinearTypes, MagicHash #-}
{-# LANGUAGE RequiredTypeArguments, TypeAbstractions #-}
module GadtOrderedUnboxedScope where

import GHC.Exts (Int(I#))
import GadtOrderedUnboxedProvider

duplicate :: Payload a b c %1 -> (Int, Int)
duplicate (Payload raw (type b) @c _) = (I# raw, I# raw)
