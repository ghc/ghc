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
import GHC.Wasm.Uint8Array

foreign import javascript unsafe "(new TextEncoder()).encode($1)"
  js_str_encode :: JSString -> JSUint8Array

foreign import javascript unsafe "(new TextDecoder('utf-8', {fatal: true})).decode($1)"
  js_to_str :: JSUint8Array -> JSString

textFromJSString :: JSString -> Text
textFromJSString str =
  let buf = js_str_encode str
  in Text (fromJSUint8Array buf) 0 (byteLength buf)

-- NOTE: Do not copy this into your program, you need to take into account the
-- offset to do this converson properly
textToJSString :: Text -> JSString
textToJSString (Text ba 0 len) = js_to_str (toJSUint8Array ba)
textToJSString _ = error "non-zero offset not supported"

foreign export javascript "main sync"
  main :: IO ()

main :: IO ()
main = T.putStrLn $ textFromJSString $ textToJSString "lorem ipsum"
