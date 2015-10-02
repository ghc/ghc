{-# OPTIONS_GHC -O2 #-}

-- Verify that -O2 is rejected when this module is loaded by GHCi
module T10549 where

import qualified Data.ByteString.Internal as Internal
import System.IO.Unsafe (unsafePerformIO)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)

type S = Ptr Word8

chr :: S -> Char
chr x = Internal.w2c $ unsafePerformIO $ peek x
