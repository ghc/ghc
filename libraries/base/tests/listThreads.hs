import Data.Maybe
import Control.Monad
import qualified Data.Set as S
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
        let lbl = "thread-"++show n
        labelThread tid lbl
        return lbl

  expectedLabels <- S.fromList <$> mapM mkThread [0..100]
  threads <- listThreads
  labels <- S.fromList . catMaybes <$> mapM threadLabel threads
  unless (S.null $ expectedLabels `S.difference` labels) $
      putStrLn $ unlines [ "thread labels don't match", show expectedLabels, show labels ]
  putMVar mvar ()

