{-# LANGUAGE RankNTypes #-}

module T1634 where

t1 :: a -> (forall b. b -> (a,b))
t1 = (,)
