{-# LANGUAGE MagicHash #-}

module Test where

import Data.Maybe
import GHC.Exts
import GHC.Internal.Wasm.Prim
import GHC.Weak
import System.Mem

type BinOp a = a -> a -> a

foreign import javascript "wrapper sync"
  js_from_hs :: BinOp Int -> IO JSVal

foreign import javascript unsafe "dynamic"
  js_to_hs :: JSVal -> BinOp Int

foreign import javascript "wrapper"
  js_mk_cont :: IO () -> IO JSVal

foreign export javascript "testDynExportFree sync"
  testDynExportFree :: Int -> Int -> Int -> IO ()

-- JSVal uses Weak# under the hood for garbage collection support,
-- this exposes the internal Weak# to observe the liveliness of
-- JSVal#. Do not use this in your own codebase since this is purely
-- an implementation detail of JSVal and subject to change!
jsvalWeak :: JSVal -> Weak JSVal
jsvalWeak (JSVal _ w _) = Weak $ unsafeCoerce# Weak w

probeWeak :: Weak v -> IO ()
probeWeak wk = print =<< isJust <$> deRefWeak wk

testDynExportFree :: Int -> Int -> Int -> IO ()
testDynExportFree x y z = do
  -- fn must be a dynamically created Haskell function closure.
  let fn a b = a * b + z
  -- wk_fn observe the liveliness of fn
  wk_fn <- mkWeak fn () Nothing
  cb <- js_from_hs fn
  -- wk_js observe the liveliness of the JavaScript callback on the
  -- Haskell heap. Make sure it's eagerly evaluated and isn't a thunk
  -- that retains cb.
  let !wk_js = jsvalWeak cb
  print $ js_to_hs cb x y
  -- Eagerly drop references to both the JavaScript callback and the
  -- Haskell function closure.
  freeJSVal cb
  performGC
  -- Now both should be dead.
  probeWeak wk_js
  probeWeak wk_fn

foreign export javascript "testDynExportGC"
  testDynExportGC :: Int -> Int -> Int -> IO JSVal

testDynExportGC :: Int -> Int -> Int -> IO JSVal
testDynExportGC x y z = do
  let fn a b = a * b + z
  wk_fn <- mkWeak fn () Nothing
  cb <- js_from_hs fn
  let !wk_js = jsvalWeak cb
  print $ js_to_hs cb x y
  -- Why performGC twice? The first run gathers some C finalizers
  -- which will be invoked in the second run to free the JSVal
  -- references. It's an implementation detail of the GHC RTS.
  performGC
  performGC
  -- Should be dead now, cb is recycled at this point.
  probeWeak wk_js
  -- Should be alive, despite cb is gone in the Haskell heap, it may
  -- still be alive in the JavaScript side so we can't drop fn!
  probeWeak wk_fn
  -- Return a continuation to be called after the JavaScript side
  -- finishes garbage collection.
  js_mk_cont $ do
    -- The JavaScript FinalizerRegistry logic only frees the stable
    -- pointer that pins fn. So we need to invoke Haskell garbage
    -- collection again.
    performGC
    -- Dead, finally.
    probeWeak wk_fn
