{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
module T14125 where

import Foreign.C.String
import Foreign.C.Types

data UnixReturn

data family IOErrno a
newtype instance IOErrno UnixReturn = UnixErrno CInt

foreign import ccall unsafe "string.h"
  strerror :: IOErrno UnixReturn -> IO CString

foreign import ccall unsafe "HsBase.h __hscore_get_errno"
  get_errno :: IO (IOErrno UnixReturn)
