-- Test that re-evaluating an AP_STACK from an interrupted async I/O call
-- does not crash. On Windows non-threaded RTS, re-entry returns EINTR
-- which readRawBufferPtr converts to IOException Interrupted. On the
-- threaded RTS (any platform), the blocking read is re-attempted and
-- succeeds because we write a byte to the pipe between evaluations.
--
-- Before the fix for #26341, re-evaluation on Windows would crash or read
-- uninitialized memory from a freed StgAsyncIOResult.
{-# OPTIONS_GHC -O -fno-full-laziness #-}

import Control.Concurrent (threadDelay, myThreadId, forkIO, killThread, rtsSupportsBoundThreads)
import Control.Exception
import Data.IORef
import Foreign
import Foreign.C
import GHC.Exts
import GHC.IO.Exception (IOErrorType(..), IOException(..))
import GHC.IO.FD (FD(..), readRawBufferPtr, writeRawBufferPtr)
import System.Info (os)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (createPipeFd)

-- Store the write fd so main can feed data into the pipe between
-- evaluations.  On Unix this unblocks the re-entered read; on Windows
-- stg_block_async returns EINTR regardless.
{-# NOINLINE writeFdRef #-}
writeFdRef :: IORef CInt
writeFdRef = unsafePerformIO $ newIORef (-1)

-- | A thunk whose unsafePerformIO blocks on a pipe read.  A forked
-- thread kills the main thread after 200ms, which creates an AP_STACK.
{-# NOINLINE blockedRead #-}
blockedRead :: ()
blockedRead = noinline unsafePerformIO $ do
    (readFd, writeFd) <- createPipeFd
    writeIORef writeFdRef writeFd
    buf <- mallocBytes 1
    mainTid <- myThreadId
    _ <- forkIO $ do
        threadDelay 200000  -- 200ms
        killThread mainTid
    -- readRawBufferPtr dispatches to asyncReadRawBufferPtr on Windows
    -- non-threaded RTS; on Unix it uses threadWaitRead + read().
    _ <- readRawBufferPtr "blockedRead" (FD readFd 0) buf 0 1
    return ()

main :: IO ()
main = do
    -- First evaluation: the thunk blocks on the pipe read, gets killed.
    catch (evaluate blockedRead)
          (\(e :: AsyncException) -> putStrLn $ "caught: " ++ show e)

    -- Write a byte so the re-entered read can complete on Unix.
    wfd <- readIORef writeFdRef
    buf <- mallocBytes 1
    poke buf 0
    _ <- writeRawBufferPtr "unblock" (FD wfd 0) buf 0 1

    -- Second evaluation: AP_STACK re-enters.
    -- Non-threaded Windows: asyncRead returns (-1, EINTR) → IOException
    -- Threaded / Unix:      read succeeds → returns normally
    let expectEINTR = os == "mingw32" && not rtsSupportsBoundThreads
    result <- try (evaluate blockedRead)
    case result of
        Left e
            | Just ioe <- fromException e
            , ioe_type (ioe :: IOException) == Interrupted
            -> putStrLn "re-evaluated ok"
            | otherwise
            -> putStrLn $ "unexpected: " ++ show e
        Right ()
            | expectEINTR -> putStrLn "unexpected: expected EINTR"
            | otherwise   -> putStrLn "re-evaluated ok"

    putStrLn "done"
