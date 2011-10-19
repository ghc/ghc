{-# LANGUAGE ForeignFunctionInterface #-}
-- | Unsafe as uses FFI with a pure function
module UnsafeInfered06_A where

foreign import ccall "math.h" sin :: CDouble -> CDouble

