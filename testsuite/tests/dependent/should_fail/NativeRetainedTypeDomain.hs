{-# LANGUAGE ExperimentalDependentTypes #-}
module NativeRetainedTypeDomain where

import Data.Kind (Type)

type Lifted = Type

type Unsupported = foreach (a :: Type). a -> a

type UnsupportedAlias = foreach (a :: Lifted) -> a -> a
