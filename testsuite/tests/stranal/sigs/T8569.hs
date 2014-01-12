{-# LANGUAGE GADTs #-}

module T8569 where

data Rep t where
  Rint :: Rep Int
  Rdata :: Rep i -> (t -> i) -> Rep t

addUp :: Rep a -> a -> Int
addUp Rint n  = n
addUp (Rdata i f) x = addUp i (f x)
