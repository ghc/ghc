module T20609d where

import Data.Word (Word8)

foreign import ccall unsafe "forall"
  forall :: IO Word8
