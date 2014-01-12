import Control.Concurrent
import Control.Exception
import Control.Monad
import System.IO.Unsafe

main :: IO ()
main = do
     -- evaluate lock -- adding this line fixes the problem

     fin1 <- newEmptyMVar
     fin2 <- newEmptyMVar

     forkIO $ ping >>= putMVar fin1
     forkIO $ ping >>= putMVar fin2

     takeMVar fin1
     takeMVar fin2

{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

ping = do
     () <- takeMVar lock
     putMVar lock ()
