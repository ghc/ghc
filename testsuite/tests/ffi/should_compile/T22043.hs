{-# LANGUAGE CApiFFI #-}
module T22043 where

import Foreign.C.Types
import Foreign.C.ConstPtr

foreign import capi "T22043.h foo"
    c_foo :: IO (ConstPtr CInt)

foreign import capi "T22043.h value bar"
    c_bar :: ConstPtr CDouble
