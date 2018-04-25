{-# LANGUAGE RankNTypes, PartialTypeSignatures #-}
module NestedExtraConstraintsWildcard where

foo :: Bool -> (Eq a, _) => a
foo = undefined
