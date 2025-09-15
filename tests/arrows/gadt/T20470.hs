{-# LANGUAGE Arrows, GADTs #-}
module T20470 where

data A where
  A :: a -> B a -> A
data B a where
  B :: B ()

foo :: A -> ()
foo = proc a ->
  case a of
    A x B ->
      id -< x
