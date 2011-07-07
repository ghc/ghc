module Dep04 where

import Foreign.Ptr
import Foreign.ForeignPtr

bad :: ForeignPtr a -> Ptr a
bad = unsafeForeignPtrToPtr

