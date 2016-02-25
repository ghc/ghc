{-# LANGUAGE PatternSynonyms #-}
-- turn on with -fwarn-missing-pattern-synonym-signatures

module Foo where

-- Should warn because of missing signature
pattern T = True

pattern J a = Just a

pattern J1 a <- Just a

pattern J2{b} = Just b

pattern J3{c} <- Just c

pattern F :: Bool
pattern F = False
