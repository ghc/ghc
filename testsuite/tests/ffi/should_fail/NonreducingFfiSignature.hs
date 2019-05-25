{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedFFITypes #-}

module NonreducingFfiSignature (c_pow) where

import Foreign.C.Types (CDouble(..))
import Data.Kind (Type)

type family Foo (x :: Type)

foreign import ccall "math.h pow"
  c_pow :: CDouble -> CDouble -> Foo Int
