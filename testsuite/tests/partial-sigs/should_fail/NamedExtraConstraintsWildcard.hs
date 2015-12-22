{-# LANGUAGE ConstraintKinds, PartialTypeSignatures, NamedWildCards #-}
module NamedExtraConstraintsWildcard where

foo :: (Eq a, _a) => a -> a
foo = undefined
