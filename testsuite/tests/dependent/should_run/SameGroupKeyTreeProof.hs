{-# LANGUAGE ExperimentalDependentTypes, GADTs, LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
module SameGroupKeyTreeProof (Tree(..), Size(..), mapValues, nodes, nodesMap, forkCount) where

import Data.Type.Equality ((:~:)(Refl), gcastWith)

data Tree k v = Tip v | Branch k (Tree k v) (Tree k v)
data Size = Zero | Succ Size

plus :: Size -> Size -> Size
plus Zero m = m
plus (Succ n) m = Succ (plus n m)

mapValues :: forall b k a. (a -> b) -> Tree k a -> Tree k b
mapValues f (Tip x) = Tip (f x)
mapValues f (Branch k l r) = Branch k (mapValues f l) (mapValues f r)

nodes :: forall a k. Tree k a -> Size
nodes (Tip _) = Zero
nodes (Branch _ l r) = plus (nodes @a @k l) (Succ (nodes @a @k r))

nodesMap :: forall k a b. foreach (f :: a -> b) -> foreach (t :: Tree k a)
         -> nodes (mapValues f t) :~: nodes t
nodesMap f = \case
    Tip _ -> Refl
    Branch _ l r -> gcastWith (nodesMap f l) (gcastWith (nodesMap f r) Refl)

forkCount :: Tree k v -> Int
forkCount = fromSize . nodes
  where
    fromSize Zero = 0
    fromSize (Succ n) = 1 + fromSize n
