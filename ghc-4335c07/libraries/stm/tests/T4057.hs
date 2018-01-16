import Control.Monad
import GHC.Conc
import System.IO

modifyTVar :: TVar Integer -> (Integer -> Integer) -> STM ()
modifyTVar t f = readTVar t >>= writeTVar t . f

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  t <- newTVarIO 0
  let f = atomically $ do always (liftM2 (<=) (readTVar t) (return 5))
                          modifyTVar t succ
  putStrLn "f1"
  f
  putStrLn "f2"
  f
  putStrLn "v"
  v <- atomically $ readTVar t
  print v
