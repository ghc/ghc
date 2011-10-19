{-# LANGUAGE ForeignFunctionInterface #-}
-- | FFI but in IO monad so safe
module SafeInfered03_A where

foreign import ccall "math.h" sin :: CDouble -> IO CDouble

