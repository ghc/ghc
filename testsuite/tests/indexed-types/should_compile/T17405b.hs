{-# LANGUAGE TypeFamilyDependencies #-}
module T17405b where

type family F2 a
type instance F2 Int = Bool
