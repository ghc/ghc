import Control.Concurrent
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
  -- TODO: Check labels
  print $ length threads
  putMVar mvar ()

