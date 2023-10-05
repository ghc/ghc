{-# LANGUAGE TypeFamilies #-}

type family F a b
type instance F a a = Int
type instance F [a] a = Bool
