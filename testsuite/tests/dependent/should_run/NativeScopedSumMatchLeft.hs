{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies, UndecidableInstances #-}
module NativeScopedSumMatchLeft (Alpha, Named, ForeignScope) where

import Data.Proxy (Proxy)

type Alpha a = some (tag :: Bool) |
  case tag of { True -> (a, Proxy tag); False -> Int }

type family LeftBody (tag :: Bool) where
  LeftBody tag = Proxy tag

type Named a = some (tag :: Bool) | (a, LeftBody tag)

type Outside = some (tag :: Bool) | Proxy tag
type ForeignScope a = some (tag :: Bool) | (a, Outside, Proxy tag)
