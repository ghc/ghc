{-# LANGUAGE TypeOperators, GADTs, EmptyCase #-}
module T2431 where

data a :~: b where
  Refl :: a :~: a

absurd :: Int :~: Bool -> a
absurd x = case x of {}
