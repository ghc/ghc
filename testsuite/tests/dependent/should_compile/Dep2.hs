{-# LANGUAGE PolyKinds, GADTs, TopLevelKindSignatures #-}

module Dep2 where

import Data.Kind (Type)

type G :: k -> Type
data G a where
  G1 :: G Int
  G2 :: G Maybe
