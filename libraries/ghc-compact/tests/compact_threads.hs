import Control.Concurrent
import Control.Monad
import GHC.Compact
import qualified Data.Map as Map
import Data.Maybe
import System.Environment

main = do
  [n] <- map read <$> getArgs
  c <- compact ()
  as <- forM [1..(n::Int)] $ \i -> async (compactAdd c (Just i))
  bs <- forM as $ \a -> async (getCompact <$> takeMVar a)
  xs <- mapM takeMVar bs
  print (sum (catMaybes xs))

async :: IO a -> IO (MVar a)
async io = do
  m <- newEmptyMVar
  forkIO (io >>= putMVar m)
  return m
