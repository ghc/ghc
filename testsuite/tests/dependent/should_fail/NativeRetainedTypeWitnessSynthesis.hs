{-# LANGUAGE ExperimentalDependentTypes, RequiredTypeArguments, ScopedTypeVariables #-}
module NativeRetainedTypeWitnessSynthesis where

import Type.Reflection (TypeRep, Typeable)

preserve :: TypeRep a -> a -> a
preserve _ x = x

missing :: forall a. a -> a
missing = preserve (type a)
