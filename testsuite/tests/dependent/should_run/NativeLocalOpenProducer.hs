{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module NativeLocalOpenProducer (Forward, Backward, Prior) where

import Data.Kind (Type)
import Foreign.C.Types (CInt (..))

foreign import ccall unsafe "stdlib.h abs" c_abs :: CInt -> CInt

type Forward :: Type -> Type
type Forward a = NativeLocalOpenProducer.forward a

forward :: a -> a
forward x = x

backward :: a -> a -> a
backward x _ = x

type Backward :: Type -> Type -> Type
type Backward a b = NativeLocalOpenProducer.backward a b

prior :: a -> a
prior x = x

$(pure [])

type Prior :: Type -> Type
type Prior a = NativeLocalOpenProducer.prior a
