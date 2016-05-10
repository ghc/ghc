{-# LANGUAGE PartialTypeSignatures, NamedWildCards #-}
module SuperCls where

f :: (Ord a, _) => a -> Bool
-- We'd like to see that the wildcard _ unifies with ()
f x = x == x

