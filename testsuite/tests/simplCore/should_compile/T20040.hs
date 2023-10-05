{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE BangPatterns        #-}

module T20040 where

import Data.Coerce

data Nat = Z | S Nat

data Vec n a where
  Nil  :: Vec 'Z a
  Cons :: a -> Vec n a -> Vec ('S n) a

newtype Succ b n = Succ { unSucc :: b (S n) }

ifoldl' :: forall b n a. (forall m. b m -> a -> b ('S m)) -> b 'Z -> Vec n a -> b n
ifoldl' f  z Nil = z
ifoldl' f !z (Cons x xs) = unSucc $ ifoldl' (\(Succ m) a -> Succ (f m a)) (Succ $ f z x) xs
