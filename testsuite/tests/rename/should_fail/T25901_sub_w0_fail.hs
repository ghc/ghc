{-# LANGUAGE ExplicitNamespaces #-}

module T25901_sub_w0_fail (T(.., data ..)) where

import GHC.Exts (IsList(type .., data ..))

data T = MkT
