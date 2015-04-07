{-# LANGUAGE RankNTypes #-}
module T10194 where

type X = forall a . a

comp :: (X -> c) -> (a -> X) -> (a -> c)
comp = (.)
