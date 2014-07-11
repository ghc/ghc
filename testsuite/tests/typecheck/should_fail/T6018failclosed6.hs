{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module T6018failclosed6 where

-- This should fail because there is no way to determine a, b and k from the RHS
type family Gc (a :: k) (b :: k) = r | r -> a b where
    Gc a b = Int
