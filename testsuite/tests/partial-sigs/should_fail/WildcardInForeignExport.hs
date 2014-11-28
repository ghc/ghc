{-# LANGUAGE PartialTypeSignatures, ForeignFunctionInterface #-}
module WildcardInForeignExport where

import Foreign.C

foreign export ccall foo :: CInt -> _
foo = undefined
