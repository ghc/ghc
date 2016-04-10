{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module T11732b where

data P1   (a :: k) = MkP1 deriving Functor
data P2 k (a :: k) = MkP2 deriving Functor

data family P1Fam (x :: y)
data family P2Fam (x :: y) (z :: w)
data instance P1Fam   (a :: k) deriving Functor
data instance P2Fam k (a :: k) deriving Functor
