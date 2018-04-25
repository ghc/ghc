{-# LANGUAGE CPP,ForeignFunctionInterface #-}

import Control.Concurrent
import Foreign.C
import System.IO

#ifdef mingw32_HOST_OS
sleep n = sleepBlock (n*1000)
foreign import stdcall unsafe "Sleep" sleepBlock :: Int -> IO ()
#else
sleep n = sleepBlock n
foreign import ccall unsafe "sleep" sleepBlock :: Int -> IO ()
#endif

main :: IO ()
main = do
     hSetBuffering stdout LineBuffering

     tid <- forkIO $ do
         putStrLn "child: Sleeping"
         _ <- sleep 1

         -- The following lines should not happen after the killThread from the
         -- parent thread completes.  However, they do...
         -- putStrLn "child: Done sleeping"
         threadDelay 100000
         putStrLn "child: Done waiting"

     threadDelay 100000
     -- putStrLn $ "parent: Throwing exception to thread " ++ show tid
     throwTo tid $ userError "Exception delivered successfully"
     putStrLn "parent: Done throwing exception"

     threadDelay 200000
