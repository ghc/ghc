{-# LANGUAGE PatternSynonyms #-}
module Qux where

-- Make sure selectors aren't generated for normal synonyms

pattern Uni a = Just a

pattern a :+: b = (a, b)

qux = a (Just True)
