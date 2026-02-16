{-# LANGUAGE CApiFFI #-}
module T26852 where

import Foreign.Ptr
import Foreign.C.Types

data Abstract

foreign import capi "T26852.h blah"
    c_blah :: Ptr (Ptr Abstract) -> IO ()

foreign import capi "T26852.h get_abstract"
    c_get_abstract :: IO (Ptr (Ptr Abstract))

foreign import capi "T26852.h get_abstract3"
    c_get_abstract3 :: IO (Ptr (Ptr (Ptr Abstract)))

foreign import capi "T26852.h get_simple"
    c_get_simple :: IO (Ptr Abstract)

foreign import capi "T26852.h get_int_pp"
    c_get_int_pp :: IO (Ptr (Ptr CInt))
