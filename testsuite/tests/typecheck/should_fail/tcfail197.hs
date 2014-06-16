{-# LANGUAGE RankNTypes #-}

module ShouldFail where

foo :: [forall a. a] -> Int
foo = error "urk"

