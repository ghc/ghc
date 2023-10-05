{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE TypeAbstractions #-}

module T9151 where

import Data.Kind
import Data.Proxy

type PEnum :: KProxy a -> Constraint
class PEnum @a kproxy where
  type ToEnum (x :: a) :: Bool
  type ToEnum x = TEHelper

type TEHelper = ToEnum Int
