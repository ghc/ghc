{-# OPTIONS_GHC -Wincomplete-patterns -Wno-missing-methods -fforce-recomp #-}
{-# LANGUAGE GADTs, TypeFamilies, PatternSynonyms #-}
module Lib where

data family T a

data instance T () where
  A :: T ()
  B :: T ()

pattern C :: T ()
pattern C = B
{-# COMPLETE A, C #-}

g :: T () -> ()
g A = ()
g C = ()

h :: T () -> ()
h C = ()
h A = ()

