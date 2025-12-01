{-# LANGUAGE PatternSynonyms, ExplicitNamespaces #-}

module T25901_sub_w (T(MkT, data ..)) where

import Data.Bool (Bool(False, data ..))

data T = MkT
