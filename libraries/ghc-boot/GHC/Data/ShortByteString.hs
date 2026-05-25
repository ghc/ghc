module GHC.Data.ShortByteString
  ( newCStringFromSBS
  ) where

import Prelude

import qualified Data.ByteString.Short as SBS
import Foreign
import Foreign.C

newCStringFromSBS :: SBS.ShortByteString -> IO CString
newCStringFromSBS sbs =
  SBS.useAsCStringLen sbs $ \(src, len) -> do
    dst <- mallocBytes (len + 1)
    copyBytes dst src len
    pokeByteOff dst len (0 :: Word8)
    pure dst
