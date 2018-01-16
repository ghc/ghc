{-# LANGUAGE PolyKinds, TypeFamilies #-}

module T9171 where
data Base

type family GetParam (p::k1) (t::k2) :: k3

type instance GetParam Base t = t

foo = undefined :: GetParam Base (GetParam Base Int)
