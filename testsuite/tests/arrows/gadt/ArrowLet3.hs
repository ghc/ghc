{-# LANGUAGE Arrows, GADTs #-}
module ArrowLet3 where

data A where
  A :: a -> B a -> A
data B a where
  B :: B ()

foo :: A -> ()
foo = proc a -> do
  let x = case a of { A x B -> x }
  id -< x
