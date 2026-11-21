{-# LANGUAGE ExperimentalDependentTypes #-}
module NativeRetainedTypeDomain where

import Data.Kind (Type)

type Lifted = Type

preserve :: foreach (a :: Type) -> a -> a
preserve _ x = x

preserveAlias :: foreach (a :: Lifted) -> a -> a
preserveAlias _ x = x
