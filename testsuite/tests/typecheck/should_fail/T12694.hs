{-# LANGUAGE GADTs #-}
module T12694 where

f :: Bool ~ Int => a -> b
f x = x
