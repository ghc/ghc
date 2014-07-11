{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances #-}

module T6018failclosed4 where

data N = Z | S N

-- this is not injective - not all injective type variables mentioned
-- on LHS are mentioned on RHS (tyvar is now nested inside a tycon)
type family KClosed (a :: N) (b :: N) = (r :: N) | r -> a b where
    KClosed (S n) m = S m
