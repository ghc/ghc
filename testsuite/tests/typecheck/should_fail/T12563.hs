{-# LANGUAGE RankNTypes #-}
module T12563 where

foo :: ((forall a. f a) -> f r) -> f r
foo g = undefined

x = \g -> foo g
