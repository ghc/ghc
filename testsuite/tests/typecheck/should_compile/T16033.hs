{-# LANGUAGE GADTs #-}
module T16033 where

f    :: (forall x. x -> forall y. y -> c) -> ()
f (_ ::  forall a. a -> forall b. b -> c)  = ()
