{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module NativeTypeCaseHiddenAlias where
import Data.Proxy
import Data.Type.Equality
type Hidden = 'Just ('Just 'True)
type F d = case d of
  Hidden -> Int
  Just _ -> Int
  Nothing -> Char
proof :: Proxy x -> F ('Just x) :~: Int
proof _ = Refl
