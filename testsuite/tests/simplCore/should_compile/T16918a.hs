{-# LANGUAGE ForeignFunctionInterface #-}
module Bug where

import Data.Word
import Foreign
import Foreign.C.String
import Foreign.C.Types

type CSaFamily = (Word16)
data SockAddr = SockAddrUnix String

pokeSockAddr :: Ptr a -> SockAddr -> IO ()
pokeSockAddr p (SockAddrUnix path) = do
    case path of
      ('\0':_) -> zeroMemory p (110)
      _        -> return ()
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p ((1) :: CSaFamily)
    let pathC = map castCharToCChar path
        poker = case path of ('\0':_) -> pokeArray; _ -> pokeArray0 0
    poker (((\hsc_ptr -> hsc_ptr `plusPtr` 2)) p) pathC

foreign import ccall unsafe "string.h" memset :: Ptr a -> CInt -> CSize -> IO ()

zeroMemory :: Ptr a -> CSize -> IO ()
zeroMemory dest nbytes = memset dest 0 (fromIntegral nbytes)
