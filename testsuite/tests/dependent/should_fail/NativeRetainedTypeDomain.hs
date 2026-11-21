{-# LANGUAGE ExperimentalDependentTypes #-}
module NativeRetainedTypeDomain where

import Data.Kind (Type)

type Lifted = Type

preserve :: foreach (a :: Type). a -> a
preserve x = x

preserveAlias :: foreach (a :: Lifted). a -> a
preserveAlias x = x
