{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures, PolyKinds, TypeFamilies #-}
module NativeTypeCaseErasedWildcard where
import Data.Kind (Type)
import Data.Proxy (Proxy)

type Discard a b = Maybe b
type Bad (m :: Maybe Type) = case m of
  Just (Discard _ b) -> Proxy b
  Nothing -> ()
