{-# LANGUAGE QuantifiedConstraints #-}
module T23333 where

foo1 :: (forall y. Bool ~ y) => z -> Bool
foo1 x = not x

foo2 :: (forall y. y ~ Bool) => z -> Bool
foo2 x = not x
