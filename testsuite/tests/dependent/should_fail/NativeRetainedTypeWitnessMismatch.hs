{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module NativeRetainedTypeWitnessMismatch where

import Data.Kind (Type)
import Type.Reflection

agree :: foreach (a :: Type) -> TypeRep a -> a -> a
agree (_ :: TypeRep actual) (_ :: TypeRep actual) value = value

wrong :: Bool
wrong = agree (typeRep @Bool) (typeRep @Int) True
