{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds, UndecidableInstances #-}

module T11255 where

type family Default :: k
type instance Default @(k1, k2) = '(Default, Default)
