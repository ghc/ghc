{-# LANGUAGE KindSignatures, GADTs, DataKinds, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module T8848 where

import qualified Control.Applicative as A
import qualified Data.Functor as Fun

data Nat = S Nat  | Z

data Shape (rank :: Nat) a where
    Nil  :: Shape Z a
    (:*) ::  a -> Shape r a -> Shape  (S r) a

instance A.Applicative (Shape Z) where
instance A.Applicative (Shape r)=> A.Applicative (Shape (S r)) where
instance Fun.Functor (Shape Z) where
instance (Fun.Functor (Shape r)) => Fun.Functor (Shape (S r)) where

map2 :: (A.Applicative (Shape r))=> (a->b ->c) -> (Shape r a) -> (Shape r b) -> (Shape r c )
map2 = \f l r -> A.pure f A.<*>  l  A.<*> r

{-# SPECIALIZE map2 :: (a->b->c)-> (Shape (S (S Z)) a )-> Shape (S (S Z)) b -> Shape (S (S Z)) c #-}

map3 :: (a->b->c)-> (Shape (S (S Z)) a )-> Shape (S (S Z)) b -> Shape (S (S Z)) c 
map3 x y z = map2 x y z