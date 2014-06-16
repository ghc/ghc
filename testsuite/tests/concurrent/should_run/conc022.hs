{-# LANGUAGE MagicHash #-}
-- !!! test tryTakeMVar

import Control.Concurrent
import Control.Exception

import GHC.Exts		( fork# )
import GHC.IO    	( IO(..) )
import GHC.Conc		( ThreadId(..) )

main = do
  m <- newEmptyMVar
  r <- timeout 5 (tryTakeMVar m) (putStrLn "timed out!" >> return Nothing)
  print (r :: Maybe Int)

  m <- newMVar True
  r <- timeout 5 (tryTakeMVar m) (putStrLn "timed out!" >> return Nothing)
  print r

timeout
   :: Int	-- secs
   -> IO a	-- action to run
   -> IO a	-- action to run on timeout
   -> IO a

timeout secs action on_timeout 
  = do
    threadid <- myThreadId
    timeout <- forkIO $ do threadDelay (secs * 1000000)
                           throwTo threadid (ErrorCall "__timeout")
    ( do result <- action
	 killThread timeout
	 return result
      ) 
      `Control.Exception.catch`
        \exception -> case fromException exception of
		       Just (ErrorCall "__timeout") -> on_timeout
		       _other -> do killThread timeout
                                    throw exception

