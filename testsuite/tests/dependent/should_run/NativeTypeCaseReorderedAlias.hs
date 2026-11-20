{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures, TypeFamilies #-}
module NativeTypeCaseReorderedAlias where
import Data.Kind (Type)
import Data.Proxy (Proxy)
type Flipped a b = Either b a
type Bad (d :: Maybe Type) = case d of
  Just (Flipped a b) -> Proxy a
  Just _ -> ()
  Nothing -> ()
