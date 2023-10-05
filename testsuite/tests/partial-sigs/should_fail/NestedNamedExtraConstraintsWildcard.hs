{-# LANGUAGE RankNTypes, PartialTypeSignatures, NamedWildCards #-}
module NestedNamedExtraConstraintsWildcard where

foo :: Bool -> (Eq a, _a) => a
foo = undefined
