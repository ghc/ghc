-- !!! test tryTakeMVar

import Control.Concurrent
import Control.Exception
import IO

import GHC.Exts		( fork# )
import GHC.IOBase	( IO(..) )
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
    timeout <- forkIOIgnoreExceptions (
			    do threadDelay (secs * 1000000)
			       throwTo threadid (ErrorCall "__timeout")
			  )
    ( do result <- action
	 killThread timeout
	 return result
      ) 
      `Control.Exception.catch`
      ( \exception -> case exception of
		       ErrorCall "__timeout" -> on_timeout		       
		       _other                -> do
						killThread timeout
						throw exception )

forkIOIgnoreExceptions :: IO () -> IO ThreadId
forkIOIgnoreExceptions action = IO $ \ s -> 
   case (fork# action_plus s) of (# s1, id #) -> (# s1, ThreadId id #)
 where
  action_plus = Control.Exception.catch action (\_ -> return ())

