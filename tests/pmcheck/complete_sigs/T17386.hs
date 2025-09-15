{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE PatternSynonyms #-}
module Lib where

data B = T | F
pattern P :: B
pattern P = T
{-# COMPLETE P, F #-}

f :: B -> ()
f P = ()

pattern Q :: B
pattern Q = T
{-# COMPLETE T, Q #-}

g :: B -> ()
g Q = ()
