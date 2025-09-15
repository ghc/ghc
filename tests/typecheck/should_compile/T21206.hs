{-# LANGUAGE QualifiedDo #-}
module T where

import Prelude hiding (pure, (>>=))

data Free f u a
  = Pure (u a)
  | forall x. Free (f u x) (forall u'. u <= u' => u' x -> Free f u' x)

pure :: u a -> Free f u a
pure = Pure
(>>=) :: Free f u a -> (forall u'. u <= u' => u' a -> Free f u' a) -> Free f u a
Pure x >>= k = k x

class f < g where
  inj :: f u a -> g u a

class u <= u' where
  inj' :: u a -> u' a

instance u <= u where
  inj' = id

send :: (f < g) => f u a -> Free g u a
send op = Free (inj op) Pure

data State s u a where
  Get :: State s u s
  Put :: u s -> State s u ()

prog () = T.do
  x <- send Get
  Pure x
