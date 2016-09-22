{-# LANGUAGE GADTs, ConstraintKinds, PolyKinds, TypeInType,  KindSignatures, RankNTypes #-}

module T12593 where

import Data.Kind

newtype Free k p a b where
   Free :: (forall q. k q => (forall c d. p c d -> q c d) -> q a b)
        -> Free k p a b

run :: k2 q => Free k k1 k2 p a b
             -> (forall (c :: k) (d :: k1). p c d -> q c d)
             -> q a b
run (Free cat) = cat
