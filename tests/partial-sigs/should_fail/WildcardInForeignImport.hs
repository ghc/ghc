{-# LANGUAGE PartialTypeSignatures, ForeignFunctionInterface #-}
module WildcardInForeignImport where

import Foreign.C

foreign import ccall "sin" c_sin :: CDouble -> _
