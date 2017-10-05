{-# LANGUAGE GADTs #-}
module T13391a where

-- this caused a panic during the work on T13391.

data A a where
  A3 :: b ~ Int => b -> A Int
