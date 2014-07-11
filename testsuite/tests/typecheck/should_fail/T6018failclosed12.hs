{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module T6018failclosed12 where

-- This should fail because there is no way to determine a, b and k from the RHS
type family Gc (a :: k) (b :: k) = r | r -> k where
    Gc a b = Int
