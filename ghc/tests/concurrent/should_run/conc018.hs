import Concurrent
import Exception

main = do
  catchAllIO (do
  	m <- newMVar ()
	putMVar m ()
     )
     (\e -> print e)
 
