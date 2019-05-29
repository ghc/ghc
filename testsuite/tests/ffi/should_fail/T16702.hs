{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE RankNTypes #-}

module T16702 where

import Foreign.C.Types
import Foreign.Ptr
import Data.Kind (Type)

foreign import ccall "math.h pow"
  c_pow :: CDouble
        -> forall (a :: Type). CDouble
        -> forall (b :: Type). CDouble

foreign import ccall "malloc"
  malloc1 :: CSize -> forall a. IO (Ptr a)

foreign import ccall "malloc"
  malloc2 :: Show a => CSize -> IO (Ptr a)

foreign import ccall "malloc"
  malloc3 :: CSize -> Show a => IO (Ptr a)
