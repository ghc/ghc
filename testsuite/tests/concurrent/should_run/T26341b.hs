-- Stress test for #26341: repeatedly interrupt async-blocked threads and
-- re-enter their AP_STACKs. Before the fix, re-entering a thunk whose
-- unsafePerformIO was blocked on an async I/O call (Windows non-threaded
-- RTS) would read uninitialized memory or free a dangling pointer,
-- because stg_block_async reserved a stack slot for a heap-allocated
-- StgAsyncIOResult that became invalid after an async exception.
--
-- This test spawns many concurrent workers, each of which:
--   1. Creates a pipe.
--   2. Builds a thunk that blocks on a pipe read via unsafePerformIO.
--   3. Evaluates the thunk and kills it with an async exception.
--   4. Re-evaluates the thunk (AP_STACK re-entry).
--   5. Repeats many times.
--
-- On threaded RTS / Unix the re-entered read succeeds (we write a byte
-- first). On Windows non-threaded RTS the re-entered async call returns
-- EINTR. Both paths exercise the fixed stack-frame layout.
{-# OPTIONS_GHC -O -fno-full-laziness #-}

import Control.Concurrent
import Control.Exception
import Foreign
import Foreign.C
import GHC.Exts
import GHC.IO.Exception (IOErrorType(..), IOException(..))
import GHC.IO.FD (FD(..), readRawBufferPtr, writeRawBufferPtr)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Internals (c_close)
import System.Process (createPipeFd)

iterations :: Int
iterations = 200

workers :: Int
workers = 4

-- Each worker independently performs `iterations` rounds of:
-- block on pipe read → interrupt → re-evaluate the AP_STACK.
worker :: Int -> MVar () -> IO ()
worker wid done = do
    buf <- mallocBytes 1
    let go 0 = return ()
        go n = do
            (readFd, writeFd) <- createPipeFd

            -- Build a fresh thunk each iteration so we get a new AP_STACK.
            let {-# NOINLINE blockedThunk #-}
                blockedThunk :: ()
                blockedThunk = noinline unsafePerformIO $ do
                    tid <- myThreadId
                    _ <- forkIO $ do
                        threadDelay 1000  -- 1ms: tight window
                        killThread tid
                    _ <- readRawBufferPtr "stress" (FD readFd 0) buf 0 1
                    return ()

            -- First evaluation: block and get killed.
            catch (evaluate blockedThunk)
                  (\(_ :: SomeException) -> return ())

            -- Write a byte so the re-entered read can complete on
            -- threaded RTS / Unix.
            poke buf 0
            _ <- writeRawBufferPtr "unblock" (FD writeFd 0) buf 0 1

            -- Second evaluation: AP_STACK re-entry.
            result <- try (evaluate blockedThunk)
            case result of
                Left e
                    | Just ioe <- fromException e
                    , ioe_type (ioe :: IOException) == Interrupted
                    -> return ()  -- expected on Windows non-threaded
                    | otherwise
                    -> throwIO (userError $
                         "worker " ++ show wid ++ " iteration " ++ show n ++
                         ": unexpected exception: " ++ show e)
                Right () -> return ()  -- expected on threaded / Unix

            -- Close the pipe fds.
            _ <- c_close readFd
            _ <- c_close writeFd

            go (n - 1)

    go iterations
    putMVar done ()

main :: IO ()
main = do
    dones <- mapM (\wid -> do
        done <- newEmptyMVar
        _ <- forkIO (worker wid done)
        return done
        ) [1..workers]

    -- Wait for all workers to finish.
    mapM_ takeMVar dones

    putStrLn "stress test passed"
    hFlush stdout
