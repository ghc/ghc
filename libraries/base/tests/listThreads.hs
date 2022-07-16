import Control.Concurrent
import Data.List (sort)
import GHC.Conc.Sync

dummyThread :: MVar () -> Int -> IO ()
dummyThread mvar n = do
  tid <- myThreadId
  labelThread tid ("thread-"++show n)
  readMVar mvar

main :: IO ()
main = do
  mvar <- newEmptyMVar
  let mkThread n = do
        tid <- forkIO $ readMVar mvar
        labelThread tid ("thread-"++show n)

  mapM_ mkThread [0..100]
  threads <- listThreads
  print $ length threads
  print . sort =<< mapM threadLabel threads
  putMVar mvar ()

