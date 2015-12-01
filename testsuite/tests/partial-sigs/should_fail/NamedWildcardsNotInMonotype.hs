{-# LANGUAGE PartialTypeSignatures, TypeFamilies, NamedWildCards, ConstraintKinds #-}
module NamedWildcardsNotInMonotype where

foo :: (Show _a, Eq _c, Eq _b) => _a -> _b -> String
foo x y = show x ++ show (x == y)
