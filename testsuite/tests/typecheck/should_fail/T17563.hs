{-# LANGUAGE QuantifiedConstraints #-}

module T17563 where

blah :: (forall a. a b ~ a c) => b -> c
blah = undefined
