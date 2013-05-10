import Control.Concurrent
import Control.Exception
import GHC.Conc

-- test forkBlockIO
main = do
  main_thread <- myThreadId
  m <- newEmptyMVar
  sub_thread <- mask_ $ forkIO $
	    	      sum [1..100000] `seq` 
                          throwTo main_thread (ErrorCall "foo")
  killThread sub_thread
  putStrLn "oops"

