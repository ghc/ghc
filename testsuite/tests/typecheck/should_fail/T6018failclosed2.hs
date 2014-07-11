{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances #-}

module T6018failclosed2 where

data N = Z | S N

-- PClosed is not injective, although the user declares otherwise. This
-- should be rejected on the grounds of calling a type family in the
-- RHS.
type family PClosed (a :: N) (b :: N) = (r :: N) | r -> a b where
    PClosed  Z    m = m
    PClosed (S n) m = S (PClosed n m)
