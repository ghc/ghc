{-# LANGUAGE ForeignFunctionInterface #-}

module A ( mkStringWriter, (<>>) ) where

import Foreign.Ptr
import Prelude

-- generated C wrappers used to use Unique values for the label
foreign import ccall "wrapper" mkStringWriter :: Int -> IO (Ptr Int)
-- make sure we properly z-encode the generated stubs
foreign import ccall "wrapper" (<>>) :: Int -> IO (Ptr Int)
