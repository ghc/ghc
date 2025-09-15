{-# LANGUAGE GADTs, DataKinds, KindSignatures, FlexibleInstances #-}

module T8537 where

import Data.Functor

data Nat = S !Nat  | Z

infixr 3 :*

data Shape (rank :: Nat) a where
    Nil  :: Shape Z a
    (:*) ::  !(a) -> !(Shape r a ) -> Shape  (S r) a

instance Functor (Shape Z) where

    fmap  = \ f Nil -> Nil
    {-# INLINABLE fmap #-}

    {-# SPECIALIZE fmap :: (Int ->Int )-> (Shape Z Int)-> (Shape Z Int) #-}
