import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import System.IO

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          replicateM_ 1000 doit

doit :: IO ()
doit = do v <- newMVar ()
          t <- forkIO (foo v)
          threadDelay 1000
          killThread t
          takeMVar v
          putChar '.'

foo :: MVar () -> IO ()
foo v = do let loop = do withMVar v $ \x -> evaluate x
                         loop
           loop
