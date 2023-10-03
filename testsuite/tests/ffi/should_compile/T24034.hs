{-# LANGUAGE ForeignFunctionInterface, CApiFFI #-}
module ShouldCompile where

import Foreign
foreign import capi "T24034.h value f" f :: FunPtr (Int -> IO ())
