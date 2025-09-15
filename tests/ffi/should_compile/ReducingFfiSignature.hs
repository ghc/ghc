{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedFFITypes #-}

module ReducingFfiSignature
  ( c_pow_1
  , c_pow_2
  , c_pow_3
  ) where

import Foreign.C.Types (CDouble(..))
import Data.Kind (Type)

type family Foo (x :: Type)

type instance Foo Int = CDouble
type instance Foo Bool = CDouble -> CDouble
type instance Foo CDouble = CDouble -> CDouble -> CDouble

foreign import ccall "math.h pow"
  c_pow_1 :: CDouble -> CDouble -> Foo Int

foreign import ccall "math.h pow"
  c_pow_2 :: CDouble -> Foo Bool

foreign import ccall "math.h pow"
  c_pow_3 :: Foo CDouble
