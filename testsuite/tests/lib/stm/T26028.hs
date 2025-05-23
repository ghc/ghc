module Main where

import GHC.Conc

forever :: IO String
forever = delay 10 >> forever

terminates :: IO String
terminates = delay 1 >> pure "terminates"

delay s = threadDelay (1000000 * s)

async :: IO a -> IO (STM a)
async a = do 
  var <- atomically (newTVar Nothing)
  forkIO (a >>= atomically . writeTVar var . Just)
  pure (readTVar var >>= maybe retry pure)

main :: IO ()
main = do
  x <- mapM async $ terminates : replicate 50000 forever
  r <- atomically (foldr1 orElse x)
  print r
