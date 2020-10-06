{-# LANGUAGE GADTs, TypeFamilies #-}

module T18809 where

data family F s t

data instance F s t where
  MkF :: {  foo :: Int } -> F Int t

bar :: F s t -> Int -> F s t
bar z y = z {foo = y}
