{-# LANGUAGE GADTs, ExistentialQuantification #-}

module ShouldFail where

data T = forall a. T a (a->Int)

f ~(T x f) = f x