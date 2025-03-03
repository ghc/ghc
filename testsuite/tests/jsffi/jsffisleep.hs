module Test where

import Control.Concurrent
import Control.Exception
import Data.Foldable
import Data.Traversable
import GHC.TopHandler
import GHC.Wasm.Prim

-- Same thing used internally by threadDelay when JSFFI is used
foreign import javascript safe "new Promise(res => setTimeout(res, $1 / 1000))"
  js_lazy_sleep :: Int -> IO ()

foreign export ccall "testWouldBlock"
  testWouldBlock :: IO ()

-- When a Haskell function exported via C FFI attempts to block on an
-- async JSFFI import, a WouldBlockException will be thrown. Also note
-- that we need to explicitly flushStdHandles, which is required for
-- non-main exports in C FFI. In JSFFI, it's always done automatically
-- for every export though.
testWouldBlock :: IO ()
testWouldBlock = catch (threadDelay 1000000) $ \(WouldBlockException err) -> do
  print $ WouldBlockException err
  flushStdHandles

foreign export javascript "testLazySleep"
  testLazySleep :: Int -> Int -> IO ()

-- If async JSFFI import blocks the caller eagerly, then this would
-- sleep for t*n milliseconds, causing a noticable delay when running
-- this test case.
testLazySleep :: Int -> Int -> IO ()
testLazySleep t n = do
  thunks <- for [1..n] $ \_ -> js_lazy_sleep t
  for_ thunks evaluate
  putStrLn "zzzzzzz"

foreign export javascript "testThreadDelay"
  testThreadDelay :: Int -> Int -> IO ()

-- Some folks may still prefer to wrap their async JSFFI imports to
-- always block on waiting for the result and only expose the wrapper
-- functions. So doing concurrent requests would require forking a
-- thread per request. Which is also fine.
testThreadDelay :: Int -> Int -> IO ()
testThreadDelay t n = do
  mvars <- for [1..n] $ \_ -> newEmptyMVar
  for_ mvars $ \mv -> forkFinally (threadDelay t) $ putMVar mv
  for_ mvars takeMVar
  putStrLn "i sleep"

foreign export javascript "testInterruptingSleep"
  testInterruptingSleep :: IO ()

-- When a thread is blocked waiting for an async JSFFI import call to
-- return, it can be interrupted by a Haskell async exception. The
-- async exception will not magically cancel the pending JavaScript
-- Promise, nor will it enqueue the target thread for execution. Only
-- when that Promise actually resolves or rejects, the thread will be
-- waken up again.
--
-- The current way JSFFI handles async exception is not completely
-- satisfactory, but it's a good enough balance between implementation
-- difficulty and ease of usage.
testInterruptingSleep :: IO ()
testInterruptingSleep = do
  mv <- newEmptyMVar
  tid <- forkIO $ catch (threadDelay 1000000 *> putMVar mv (Right ())) $ \(e :: SomeException) -> putMVar mv $ Left e
  -- Without this, the thread we forked earlier likely have not even
  -- performed the call yet. It will be interrupted and silently
  -- dropped without a chance to be waken up, so the MVar will remain
  -- empty and this thread will also be suspended without waking up,
  -- eventually propagating as a "never fulfilled top-level await"
  -- error to the JavaScript host.
  threadDelay 1000
  killThread tid
  print =<< takeMVar mv
