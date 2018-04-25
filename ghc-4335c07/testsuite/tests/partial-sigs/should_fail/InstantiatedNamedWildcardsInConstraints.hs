{-# LANGUAGE NamedWildCards #-}
module InstantiatedNamedWildcardsInConstraints where

foo :: (Enum _a, _) => _a -> (String, b)
foo x = (show (succ x), x)
