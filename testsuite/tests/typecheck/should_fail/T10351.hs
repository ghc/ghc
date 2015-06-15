module T10351 where

class C a where
  op :: a -> ()

f x = op [x]
