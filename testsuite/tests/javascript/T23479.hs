{-# LANGUAGE MagicHash #-}

import GHC.Prim

import GHC.JS.Prim

foreign import javascript "((x) => { console.log(x); })"
  js_log1 :: JSVal -> IO ()

main :: IO ()
main = do
  js_log1 (JSVal (unsafeUnpackJSStringUtf8## test_addr_1))
  where
    test_addr_1 :: Addr#
    test_addr_1 = "test_val_1"#
