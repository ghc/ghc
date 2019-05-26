{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE RankNTypes #-}

module NonleadingForallFfiSignature (c_pow) where

import Foreign.C.Types (CDouble(..))
import Data.Kind (Type)

foreign import ccall "math.h pow"
  c_pow :: CDouble
        -> forall (a :: Type). CDouble
        -> forall (b :: Type). CDouble
