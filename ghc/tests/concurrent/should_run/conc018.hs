import Concurrent
import Exception

main = do
  Exception.catch (do
  	m <- newMVar ()
	putMVar m ()
     )
     (\e -> print e)
 
