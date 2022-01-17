{-# LANGUAGE CApiFFI #-}

module Foo where

import Foreign.Ptr
import Foreign.C

foreign import capi unsafe "T15531.h fn4" c_fn4 :: CChar -> IO CChar

foreign import capi unsafe "T15531.h fn5" c_fn5 :: Ptr CChar -> IO (Ptr CChar)

foreign import capi unsafe "T15531.h fn6" c_fn6 :: Ptr (Ptr CChar) -> IO (Ptr (Ptr CChar))


foreign import capi unsafe "T15531.h fn7" c_fn7 :: CUChar -> CSChar -> CShort -> CUShort -> CInt -> CUInt -> CLong -> CULong -> CSize -> IO ()


foreign import capi unsafe "T15531.h fn8" c_fn8 :: Ptr CUChar -> Ptr CSChar -> Ptr CShort -> Ptr CUShort -> Ptr CInt -> Ptr CUInt -> Ptr CLong -> Ptr CULong -> Ptr CSize -> IO ()

