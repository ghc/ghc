{-# LANGUAGE TypeFamilies, TypeAbstractions #-}
module T23512b where
import GHC.Types

type family F2 a :: k
type instance F2 @(j -> j) Int = Any :: j -> j

type family F3 a :: k
type instance forall j. F3 Int = Any :: j -> j
