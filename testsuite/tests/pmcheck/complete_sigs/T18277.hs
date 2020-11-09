{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE PatternSynonyms #-}

module Lib where

type List = []

pattern DefinitelyAString :: String -> String
pattern DefinitelyAString x = x
{-# COMPLETE DefinitelyAString #-}

f :: String -> String
f (DefinitelyAString x) = x
