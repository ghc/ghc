{-# LANGUAGE ForeignFunctionInterface, CApiFFI #-}
module ShouldCompile where

import Foreign
foreign import capi "cc018.h value f" f :: FunPtr (Int -> IO ())
