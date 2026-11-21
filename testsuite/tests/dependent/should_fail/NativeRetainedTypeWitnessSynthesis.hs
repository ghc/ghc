{-# LANGUAGE ExperimentalDependentTypes, RequiredTypeArguments, ScopedTypeVariables #-}
module NativeRetainedTypeWitnessSynthesis where

import Data.Kind (Type)
import Type.Reflection (TypeRep, Typeable)

preserve :: TypeRep a -> a -> a
preserve _ x = x

missing :: forall a. a -> a
missing = preserve (type a)

selected :: foreach (a :: Type). a -> String
selected _ = show a

missingInvisible :: forall a. a -> String
missingInvisible = selected
