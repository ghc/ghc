{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module NativeLocalOpenProducer (Forward, Backward, Prior, Apply) where

import Data.Kind (Type)
import Foreign.C.Types (CInt (..))

foreign import ccall unsafe "stdlib.h abs" c_abs :: CInt -> CInt

type Forward :: Type -> Type
type Forward a = NativeLocalOpenProducer.forward a

{-# NOINLINE forward #-}
forward x = helper x

helper, outside :: a -> a
helper x = x
outside x = x

backward :: a -> a -> a
backward x y = route x y

route x y = select x y

select :: a -> a -> a
select x _ = x

type Backward :: Type -> Type -> Type
type Backward a b = backward a b

type Apply :: (Type -> Type) -> Type -> Type
type Apply backward a = backward a

prior :: a -> a
prior x = forward x

$(pure [])

type Prior :: Type -> Type
type Prior a = NativeLocalOpenProducer.prior a
