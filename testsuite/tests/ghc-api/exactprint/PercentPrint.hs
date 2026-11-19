{-# LANGUAGE DataKinds, LinearTypes, Modifiers, UnicodeSyntax #-}
module PercentPrint where

import GHC.Exts (U, Multiplicity(One))

type OneArrow a b  = a %U %One -> b
type AlsoArrow a b = a %One {- index -} %U → b
type Unicode a b   = a %U ⊸ b
