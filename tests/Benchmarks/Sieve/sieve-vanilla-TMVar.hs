import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent
import Control.Exception
import System.Environment
import Data.IORef

-- Map over [2..] (2 until infinity), putting the value in mOut. The putting operation will block until
-- mOut is empty. mOut will become empty when some other thread executes takeTMVar (getting its value).
generate :: TMVar Int -> IO ()
generate mOut = mapM_ (\v -> atomically $ putTMVar mOut v) [2..]

-- Take a value from mIn, divide it by a prime, if the remainder is not 0, put the value in mOut.
primeFilter :: TMVar Int -> TMVar Int -> Int -> IO ()
primeFilter mIn mOut prime = do
  forever $ do
    i <- atomically $ takeTMVar mIn
    when (i `mod` prime /= 0) (atomically $ putTMVar mOut i)

-- Take the first commandline argument and call it numArg.
-- Create a new mVar and call it mIn and spawn a thread that runs generate on mIn.
-- Read numArg as an integer value, and run newEmptyTMVar that amount of times,
-- calling the result out.
-- Fold over the elements of out, with the function linkFilter, having mIn as the first value.
main = do
  numArg:_ <- getArgs
  mIn <- atomically $ newEmptyTMVar
  forkIO $ generate mIn
  out <- replicateM (read numArg) (atomically newEmptyTMVar)
  foldM_ linkFilter mIn out

-- Take a value from mIn, and call it prime. Then show that prime. Make a new thread that
-- runs primeFilter with mIn, mOut and the prime. When this function is used as a fold
-- function, mOut becomes the mIn of the next iteration.
linkFilter :: TMVar Int -> TMVar Int -> IO (TMVar Int)
linkFilter mIn mOut = do
  prime <- atomically $ takeTMVar mIn
  putStrLn $ show prime
  forkIO $ primeFilter mIn mOut prime
  return mOut
