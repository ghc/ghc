{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds, GADTs #-}

module T8707 where

import Data.Kind

data family SingDF (a :: (k, k2 -> Type))
data Ctor :: k -> Type

data instance SingDF (a :: (Bool, Bool -> Type)) where
  SFalse :: SingDF '(False, Ctor)
