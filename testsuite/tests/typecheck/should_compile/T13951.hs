{-# LANGUAGE PolyKinds, GADTs, Rank2Types, ScopedTypeVariables, Trustworthy #-}
module Control.Monad.Skeleton.Internal where

data Cat k a b where
  Empty :: Cat k a a
  Leaf :: k a b -> Cat k a b
  Tree :: Cat k a b -> Cat k b c -> Cat k a c

viewL :: forall k a b r. Cat k a b
  -> ((a ~ b) => r)
  -> (forall x. k a x -> Cat k x b -> r)
  -> r
viewL Empty e _ = e
viewL (Leaf k) _ r = k `r` Empty
viewL (Tree a b) e r = go a b where
  go :: Cat k a x -> Cat k x b -> r
  go Empty t = viewL t e r
  go (Leaf k) t = r k t
  go (Tree c d) t = go c (Tree d t)
