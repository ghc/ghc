-- !!! test tryTakeMVar

import Concurrent
import Exception
import IO

import PrelIOBase
import PrelConc
import PrelGHC

main = do
  m <- newEmptyMVar
  r <- timeout 5 (tryTakeMVar m) (putStrLn "timed out!" >> return Nothing)
  print (r :: Maybe Int)


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
      `Exception.catch`
      ( \exception -> case exception of
		       ErrorCall "__timeout" -> on_timeout		       
		       _other                -> do
						killThread timeout
						throw exception )

forkIOIgnoreExceptions :: IO () -> IO ThreadId
forkIOIgnoreExceptions action = IO $ \ s -> 
   case (fork# action_plus s) of (# s1, id #) -> (# s1, ThreadId id #)
 where
  action_plus = Exception.catch action (\_ -> return ())

