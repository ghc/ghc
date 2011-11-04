import Control.Concurrent
import Control.Monad.Fix

data Client = Client
	{ clientLock :: MVar ()
	}

main = do
	mvar <- newMVar ()
	
	client <- mfix $ \client -> do
		_ <- forkIO (mainLoop client)
                threadDelay 200000
		return (Client mvar)
	return ()

mainLoop client = withMVar (clientLock client) (\_ -> return ())
