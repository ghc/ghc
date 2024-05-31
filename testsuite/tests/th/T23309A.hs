module T23309A (c_foo) where

import Foreign.C.String
import Foreign.C.Types

foreign import ccall unsafe "foo" c_foo :: CInt -> IO CString
