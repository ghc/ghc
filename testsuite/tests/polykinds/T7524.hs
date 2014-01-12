{-# LANGUAGE TypeFamilies, PolyKinds #-}
module T7524 where

type family F (a :: k1) (b :: k2)
type instance F a a = Int
type instance F a b = Bool
