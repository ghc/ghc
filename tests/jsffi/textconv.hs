-- This test demonstrates how to convert between Text and JSString.
-- Ideally this would be a part of GHC.Wasm.Prim, but that module
-- can't use anything from text. Hopefully the code here can be
-- properly adopted somewhere else.

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

import Data.Array.Byte
import Data.Coerce
import Data.Text.Internal
import qualified Data.Text.IO.Utf8 as T
import GHC.Exts
import GHC.IO
import GHC.Wasm.Prim

newtype JSUint8Array = JSUint8Array JSVal

foreign import javascript unsafe "(new TextEncoder()).encode($1)"
  js_str_encode :: JSString -> IO JSUint8Array

foreign import javascript unsafe "$1.byteLength"
  js_buf_len :: JSUint8Array -> IO Int

foreign import javascript unsafe "(new Uint8Array(__exports.memory.buffer, $2, $1.byteLength)).set($1)"
  js_from_buf :: JSUint8Array -> Ptr a -> IO ()

foreign import javascript unsafe "(new TextDecoder('utf-8', {fatal: true})).decode(new Uint8Array(__exports.memory.buffer, $1, $2))"
  js_to_str :: Ptr a -> Int -> IO JSString

textFromJSString :: JSString -> Text
textFromJSString str = unsafeDupablePerformIO $ do
  buf <- js_str_encode str
  I# len# <- js_buf_len buf
  IO $ \s0 -> case newByteArray# len# s0 of
    (# s1, mba# #) -> case unIO (js_from_buf buf (Ptr (mutableByteArrayContents# mba#))) s1 of
      (# s2, _ #) -> case unIO (freeJSVal (coerce buf)) s2 of
        (# s3, _ #) -> case unsafeFreezeByteArray# mba# s3 of
          (# s4, ba# #) -> (# s4, Text (ByteArray ba#) 0 (I# len#) #)

textToJSString :: Text -> JSString
textToJSString (Text (ByteArray ba#) (I# off#) (I# len#)) = unsafeDupablePerformIO $
  IO $ \s0 -> case newPinnedByteArray# len# s0 of
    (# s1, mba# #) -> case copyByteArray# ba# off# mba# 0# len# s1 of
      s2 -> keepAlive# mba# s2 $ unIO $ js_to_str (Ptr (mutableByteArrayContents# mba#)) $ I# len#

foreign export javascript "main sync"
  main :: IO ()

main :: IO ()
main = T.putStrLn $ textFromJSString $ textToJSString "lorem ipsum"
