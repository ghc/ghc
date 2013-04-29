import Control.Monad
import LwConc.Substrate
import ConcurrentList
import MVarList
import System.Environment
import Data.IORef

initSched = do
  newSched
  n <- getNumCapabilities
  replicateM_ (n-1) newCapability

-- Map over [2..] (2 until infinity), putting the value in mOut. The putting operation will block until
-- mOut is empty. mOut will become empty when some other thread executes takeMVar (getting its value).
generate :: MVar Int -> IO ()
generate mOut = mapM_ (putMVar mOut) [2..]

-- Take a value from mIn, divide it by a prime, if the remainder is not 0, put the value in mOut.
primeFilter :: MVar Int -> MVar Int -> Int -> IO ()
primeFilter mIn mOut prime = do
  forever $ do
    i <- takeMVar mIn
    when (i `mod` prime /= 0) (putMVar mOut i)

-- Take the first commandline argument and call it numArg.
-- Create a new mVar and call it mIn and spawn a thread that runs generate on mIn.
-- Read numArg as an integer value, and run newEmptyMVar that amount of times,
-- calling the result out.
-- Fold over the elements of out, with the function linkFilter, having mIn as the first value.
main = do
  initSched
  numArg:_ <- getArgs
  mIn <- newEmptyMVar
  forkIO $ generate mIn
  out <- replicateM (read numArg) newEmptyMVar
  foldM_ linkFilter mIn out

-- Take a value from mIn, and call it prime. Then show that prime. Make a new thread that
-- runs primeFilter with mIn, mOut and the prime. When this function is used as a fold
-- function, mOut becomes the mIn of the next iteration.
linkFilter :: MVar Int -> MVar Int -> IO (MVar Int)
linkFilter mIn mOut = do
  prime <- takeMVar mIn
  debugPrint $ show prime
  forkIO $ primeFilter mIn mOut prime
  return mOut
