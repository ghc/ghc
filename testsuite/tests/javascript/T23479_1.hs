{-# LANGUAGE MagicHash #-}
module Main where

import GHC.Prim
import GHC.JS.Prim

foreign import javascript "((x) => { console.log(x); })"
  js_log1 :: JSVal -> IO ()

test_val_2 :: String
test_val_2 = "test_val_2"

test_val_80_global :: String
test_val_80_global = "test_val_80_globaltest_val_80_globaltest_val_80_globaltest_val_80_globaltest_val"

main :: IO ()
main = do
  -- Direct usage
  js_log1 (JSVal (unsafeUnpackJSStringUtf8## "test_val_1"#))
  -- Requires string sinker hit for strings shorter 80 symbols
  js_log1 (toJSString test_val_2)
  -- Requires rewrite hit "toJSString/literal"
  js_log1 (toJSString test_val_3)
  -- Locally defined strings become unfloatted at any length
  js_log1 (toJSString test_val_80_local)
  -- Globally defined strings with length >= 80 should not be unfloatted
  js_log1 (toJSString test_val_80_global)
  where
    test_val_3 :: String
    test_val_3 = "test_val_3"

    test_val_80_local :: String
    test_val_80_local = "test_val_80_localtest_val_80_localtest_val_80_localtest_val_80_localtest_val_80_"
