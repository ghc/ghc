import Data.IORef
import Control.Monad
import Control.Exception
import Control.Concurrent.MVar
import System.Mem

main :: IO ()
main = do
  run
  run
  run
  run
  m <- newEmptyMVar
  quit m
  performMajorGC
  takeMVar m

run :: IO ()
run = do
  ref <- newIORef ()
  void $ mkWeakIORef ref $ do
    putStr "."
    throwIO $ ErrorCall "failed"

quit :: MVar () -> IO ()
quit m = do
  ref <- newIORef ()
  void $ mkWeakIORef ref $ do
    putMVar m ()
