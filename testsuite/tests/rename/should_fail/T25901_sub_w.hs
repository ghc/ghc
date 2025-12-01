{-# LANGUAGE ExplicitNamespaces #-}

module T25901_sub_w (T(MkT, data ..)) where

import Data.Bool (Bool(False, data ..))
import GHC.Exts (IsList(type .., data ..))

data T = MkT
