{-# LANGUAGE DataKinds, PolyKinds #-}

module T9632 where

import Data.Kind

data B = T | F
data P :: B -> Type

type B' = B
data P' :: B' -> Type
