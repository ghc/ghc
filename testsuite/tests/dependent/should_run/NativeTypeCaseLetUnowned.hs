{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds #-}
module NativeTypeCaseLetUnowned where
import Data.Proxy (Proxy)
type Capture b = let n = Proxy 'Nothing in case b of
  'True  -> n
  'False -> Maybe n

type Nested b =
  let outer = Proxy 'Nothing in
  let inner = outer in
  case b of
    'True  -> inner
    'False -> Maybe inner
