import Concurrent

main = do
  catch (do
  	m <- newMVar ()
	putMVar m ()
     )
     (\e -> print e)
 
