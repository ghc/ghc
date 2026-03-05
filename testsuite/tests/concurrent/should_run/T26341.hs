{-# OPTIONS_GHC -O -fno-full-laziness #-}

import Control.Concurrent (threadDelay, myThreadId, forkIO, killThread)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception
import GHC.Exts

compute :: Int
compute = noinline unsafePerformIO $ do
    mainThreadID <- myThreadId
    _ <- forkIO $ do
        threadDelay 500000
        killThread mainThreadID
    threadDelay 1000000
    return 0

main = do
    catch (print compute) (\(e :: AsyncException) -> print $ "1:" ++ show e)
    catch (print compute) (\(e :: AsyncException) -> print $ "2:" ++ show e)
    print "done"
