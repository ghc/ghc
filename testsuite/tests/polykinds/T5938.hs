{-# LANGUAGE PolyKinds, TypeFamilies, DataKinds #-}
module T5938 where

import Data.Kind (Type)

type family KindFam (a :: k)
type instance KindFam (a :: Type) = Int
type instance KindFam (a :: Bool) = Bool
type instance KindFam (a :: Maybe k) = Char -- doesn't work
