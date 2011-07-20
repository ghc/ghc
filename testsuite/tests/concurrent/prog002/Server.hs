import Scheduler
import Foreign
import Foreign.C
import System.Random
import Control.Concurrent

expensive = f (500 :: Int)
  where f 0 = stop
	f n = do
              r <- atom $ getStdRandom (randomR (0,99::Int))
              r `seq` f $! n-1

main = do
  m <- newEmptyMVar
  forkIO (do 
	  runTIO $ map (\x->expensive) [1..500]
	  putMVar m ())
  takeMVar m
