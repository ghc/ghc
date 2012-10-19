{-# LANGUAGE GADTs, RankNTypes #-}

module ShouldCompile where

-- Checks for bindInstsOfPatId
f :: (forall a. Eq a => a -> a) -> Bool
f g = g True