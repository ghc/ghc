module Cgrun067A (miscompiledFn) where

import Foreign.C
import Foreign

miscompiledFn :: CString -> IO String
miscompiledFn cp = do
  l <- lengthArray0 0 cp
  if l <= 0 then return "" else loop "" (l-1)
  where
    loop s i = do
        xval <- peekElemOff cp i
        let val = castCCharToChar xval
        val `seq` if i <= 0 then return (val:s) else loop (val:s) (i-1)
