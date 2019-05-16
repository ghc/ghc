{-# LANGUAGE PatternSynonyms #-}

module Lib where

data T = A | B | C
  deriving Eq

pattern BC :: T
pattern BC = C

{-# COMPLETE A, BC #-}

f A  = 1
f B  = 2
f BC = 3
f _  = error "impossible"
