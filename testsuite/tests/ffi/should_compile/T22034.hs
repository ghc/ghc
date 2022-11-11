{-# LANGUAGE CApiFFI #-}
module T22034 where

import Foreign.C.Types

foreign import capi "T22034.h foo"
    c_foo :: IO (ConstPtr CInt)

foreign import capi "T22034.h value bar"
    c_bar :: ConstPtr CDouble
