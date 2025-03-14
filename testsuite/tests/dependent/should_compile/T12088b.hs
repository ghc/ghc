{-# LANGUAGE DataKinds, TypeFamilies #-}

module T12088b where

import Data.Kind

type family IxKind (m :: Type) :: Type
type family Value (m :: Type) :: IxKind m -> Type
data T (k :: Type) (f :: k -> Type) = MkT

type instance IxKind (T k f) = k
type instance Value (T k f) = f