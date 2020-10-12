{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module T18831 where

import Data.Kind
import Data.Proxy

data T1 :: Proxy 0 -> Type
data T2 :: () => Type
data T3 :: (Type :: Type) -> Type
data T4 :: Proxy '[Type,Type] -> Type
data T5 :: Proxy '(Type,Type) -> Type
