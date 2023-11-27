-- [ ghc-Bugs-1249226 ] runInteractiveProcess and closed stdin.
-- Fixed in rev  1.9 of fptools/libraries/base/cbits/runProcess.c

-- This test doesn't work in GHCi, because FD 0 gets re-allocated to
-- the IO manager pipe, which isn't set to non-blocking mode, and the
-- interactive prompt ends up blocking on a read from this descriptor.

import System.IO
import Control.Concurrent
import System.Process

main = do
  hClose stdin -- everything works as expected if the handle isn't closed.
  putStrLn "Running cat ..."
  (inp, out, err, pid) <- runInteractiveProcess "cat" [] Nothing Nothing
  forkIO (hPutStrLn inp "foo" >> hClose inp)
  mout <- newEmptyMVar
  merr <- newEmptyMVar
  forkIO (hGetContents out >>= \s -> length s `seq` putMVar mout s)
  forkIO (hGetContents err >>= \s -> length s `seq` putMVar merr s)
  -- Don't want to deal with waitForProcess and -threaded right now.
  takeMVar mout >>= putStrLn
  takeMVar merr >>= putStrLn
  return ()
