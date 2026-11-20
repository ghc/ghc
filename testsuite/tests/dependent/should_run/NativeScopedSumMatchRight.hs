{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies, UndecidableInstances #-}
module NativeScopedSumMatchRight (Alpha, Named, ForeignScope) where

import Data.Proxy (Proxy)

type Alpha a = some (flag :: Bool) |
  case flag of { True -> (a, Proxy flag); False -> Int }

type family RightBody (flag :: Bool) where
  RightBody flag = Proxy flag

type Named a = some (flag :: Bool) | (a, RightBody flag)

type Outside = some (flag :: Bool) | Proxy flag
type ForeignScope a = some (flag :: Bool) | (a, Outside, Proxy flag)
