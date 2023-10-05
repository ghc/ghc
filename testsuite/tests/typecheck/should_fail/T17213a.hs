{-# LANGUAGE RankNTypes #-}
module T17213a where

foo :: (forall a. a->a)-> Int
foo x = error "ukr"
