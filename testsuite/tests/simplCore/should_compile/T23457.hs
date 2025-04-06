{-
This test sketches out an example application for #23457 from the
bytestring package (https://github.com/haskell/bytestring/issues/558).

The key point is that the first component of a function like 'uncons'
ought to perform its read eagerly when possible, since just reading
the first Word8 of a non-empty ByteString is likely to be cheaper that
creating a thunk to defer the work of reading that Word8, /even if the
result of performing that read is never used!/

But if we do so, we really ought to be able to discard the underlying
'readWord8OffAddr#' operation in cases where the resulting Word8 is
obviously statically dead/unused, such as in the function 'testFun' below.
-}

{-# LANGUAGE MagicHash, UnboxedTuples #-}
module T23457 where

import Data.Word
import GHC.Exts (realWorld#)
import GHC.ForeignPtr (ForeignPtr, plusForeignPtr, unsafeWithForeignPtr)
import GHC.IO
import Foreign.Storable

import Prelude (Int, Maybe(..), Num(..), Ord(..), ($))

data ByteString
  = BS {-# UNPACK #-} !(ForeignPtr Word8) -- contents
       {-# UNPACK #-} !Int -- length

uncons :: ByteString -> Maybe (Word8, ByteString)
uncons (BS p n)
  | n <= 0
  = Nothing
  | !h <- accursedUnutterablePerformIO $ unsafeWithForeignPtr p peek
  = Just (h, BS (plusForeignPtr p 1) (n-1))

testFun :: ByteString -> Int
testFun bs = case uncons bs of
  Nothing -> 24
  Just (_, BS _ len) -> len

accursedUnutterablePerformIO :: IO t -> t
accursedUnutterablePerformIO (IO t) = case t realWorld# of (# _, r #) -> r
