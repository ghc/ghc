{-# LANGUAGE ExperimentalDependentTypes, NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE RequiredTypeArguments, Safe, ScopedTypeVariables, TypeApplications #-}
module Main where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import NativeRetainedTypeWitnessLib
import Type.Reflection (TypeRep, Typeable)

-- | An abstract operand needs a dictionary from its caller.
reflect :: forall a. Typeable a => a -> Maybe a
reflect = onlyBool (type a)

erased :: forall (a :: Type) -> ()
erased (type _) = ()

ignoreType :: forall (a :: Type). Proxy a -> ()
ignoreType _ = erased (type a)

main :: IO ()
main = do
  print (preserve (type Bool) True, reflect False)
  print (onlyBool (type Char) (error "unused payload"))
  print (ignoreType (Proxy @Char), (type Maybe) :: TypeRep Maybe)
