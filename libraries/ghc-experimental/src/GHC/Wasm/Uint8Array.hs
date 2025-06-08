{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- | Utilities to marshall a javascript 'Uint8Array' to a 'ByteArray'
module GHC.Wasm.Uint8Array
  ( toJSUint8Array, fromJSUint8Array, JSUint8Array(..), byteLength
  ) where

import Data.Array.Byte
import GHC.Wasm.Prim
import GHC.Ptr
import GHC.Exts
import System.IO.Unsafe
import GHC.IO

newtype JSUint8Array = JSUint8Array { unJSUint8Array :: JSVal }

foreign import javascript unsafe
  "$1.byteLength"
  byteLength :: JSUint8Array -> Int

withByteArrayContents :: ByteArray -> (Ptr () -> Int -> IO a) -> IO a
withByteArrayContents (ByteArray arr#) f = IO $ \s0 ->
  keepAlive# arr# s0 (unIO $ f (Ptr (byteArrayContents# arr#)) (I# (sizeofByteArray# arr#) ))

fromJSUint8ArrayLen :: Int -> JSUint8Array -> ByteArray
fromJSUint8ArrayLen (I# len) arr = unsafePerformIO $ IO $
  \s -> case newByteArray# len s of
    (# s', mb #) ->
      let a = mutableByteArrayContents# mb
      in case unIO (memorySetUint8Array (Ptr a) (I# len) arr) s' of
            (# s'', () #) ->  case unsafeFreezeByteArray# mb s'' of
                                (# s''', arr' #) -> (# s''', ByteArray arr' #)

toJSUint8Array :: ByteArray -> JSUint8Array
toJSUint8Array ba =
  unsafePerformIO $ withByteArrayContents ba uint8ArrayFromMemory

fromJSUint8Array :: JSUint8Array -> ByteArray
fromJSUint8Array src_buf = fromJSUint8ArrayLen (byteLength src_buf) src_buf

foreign import javascript unsafe
  "(new Uint8Array(__exports.memory.buffer, $1, $2)).set($3)"
  memorySetUint8Array :: Ptr a -> Int -> JSUint8Array -> IO ()

foreign import javascript unsafe
  "new Uint8Array(new Uint8Array(__exports.memory.buffer, $1, $2))"
  uint8ArrayFromMemory :: Ptr a -> Int -> IO JSUint8Array

