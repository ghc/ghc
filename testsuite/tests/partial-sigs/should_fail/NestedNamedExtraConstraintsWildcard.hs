{-# LANGUAGE RankNTypes, PartialTypeSignatures, NamedWildcards #-}
module NestedNamedExtraConstraintsWildcard where

foo :: Bool -> (Eq a, _a) => a
foo = undefined
