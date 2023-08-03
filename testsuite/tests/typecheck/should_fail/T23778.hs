{-# LANGUAGE TypeFamilies, NoPolyKinds #-}
module T23778 where

import Data.Kind

data family D :: Type -> Type
data instance forall d u v. D u = MkD1 | MkD2 u
