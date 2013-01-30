{-# LANGUAGE ForeignFunctionInterface #-}
module T where

import Foreign
foreign import ccall "f" f :: FunPtr (Int -> IO ())
