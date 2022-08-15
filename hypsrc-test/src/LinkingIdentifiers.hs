{-# LANGUAGE Haskell2010 #-}
-- Tests that the identifiers/operators are properly linked even when:
--
--   * backquoted, parenthesized, vanilla
--   * qualified, not-qualified
--
module LinkingIdentifiers where

ident :: Int -> Int -> Int
x `ident` 2 = (x `ident` 2) + (x `LinkingIdentifiers.ident` 2)
ident x 2 = ident x 2 + LinkingIdentifiers.ident x 2

(++:++) :: Int -> Int -> Int
x ++:++ 2 = (x ++:++ 2) + (x LinkingIdentifiers.++:++ 2)
(++:++) x 2 = (++:++) x 2 + (LinkingIdentifiers.++:++) x 2
