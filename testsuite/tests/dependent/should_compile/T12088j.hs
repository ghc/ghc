{-# LANGUAGE TypeFamilies, DataKinds #-}
module T12088j where

type family Open a
type instance Open Int = Bool

type family F a :: Open a
type instance F Int = True
