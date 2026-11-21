{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
module NativeRetainedTypeWitnessSynthesis where

import Data.Kind (Type)

preserve :: foreach (a :: Type) -> a -> a
preserve _ x = x

wrong :: Bool
wrong = preserve (type Bool) True
