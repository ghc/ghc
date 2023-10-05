{-# LANGUAGE PolyKinds, GADTs, KindSignatures, DataKinds, FlexibleInstances #-}

module T7438a where

import Data.Kind (Type)

data Thrist :: k -> k -> Type where
  Nil :: Thrist a a


