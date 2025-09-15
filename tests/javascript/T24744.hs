module Main where

import GHC.Prim
import GHC.JS.Prim

foreign import javascript "((x) => { console.log(x); })"
  js_log1 :: JSVal -> IO ()

main :: IO ()
main = do
  -- When long string (>= 80) used once it is unfloatted
  js_log1 (toJSString test_val_80_local_once)

  -- When long string (>= 80) used more than once no unfloatting happened
  js_log1 (toJSString test_val_80_local)
  js_log1 (toJSString (testFn80 "testFn80:"))

  -- Even if short string used more than once it is unfloatted anyway
  js_log1 (toJSString test_val_1)
  js_log1 (toJSString (testFn "testFn:"))
  where
    test_val_80_local_once :: String
    test_val_80_local_once = "test_val_80_local_oncetest_val_80_local_oncetest_val_80_local_oncetest_val_80_lo"

    test_val_80_local :: String
    test_val_80_local = "test_val_80_localtest_val_80_localtest_val_80_localtest_val_80_localtest_val_80_"

    testFn80 s = s ++ test_val_80_local
    -- We should mark this function as NOINLINE to prevent deeper optimizations for the specific test case
    {-# NOINLINE testFn80 #-}

    test_val_1 :: String
    test_val_1 = "test_val_1"

    testFn s = s ++ test_val_1
    -- We should mark this function as NOINLINE to prevent deeper optimizations for the specific test case
    {-# NOINLINE testFn #-}
