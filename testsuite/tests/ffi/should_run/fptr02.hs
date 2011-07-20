{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign
import Control.Monad

foreign import ccall "&free" pfree :: FunPtr (Ptr a -> IO ())

main = replicateM_ 1000000 $ newForeignPtr pfree nullPtr
