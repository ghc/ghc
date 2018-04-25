{-# LANGUAGE PolyKinds, FunctionalDependencies, MultiParamTypeClasses #-}

module T9201 where

class MonoidalCCC (f :: x -> y) (d :: y -> y -> *) | f -> d where
  ret :: d a (f a)
