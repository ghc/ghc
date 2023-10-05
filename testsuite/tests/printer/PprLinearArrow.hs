{-# LANGUAGE LinearTypes, DataKinds, UnicodeSyntax #-}

module PprLinearArrow where

import GHC.Types (Multiplicity(One, Many))

n1 :: a %1 -> b
n1 = undefined

u1 :: a %1 → b
u1 = undefined

n2 :: a  %(Many) -> b
n2 = undefined

u2 :: a  %(Many) → b
u2 = undefined

m3 :: a ⊸ b
m3 = undefined

n4 :: a %p  -> b
n4 = undefined

u4 :: a %p  → b
u4 = undefined
