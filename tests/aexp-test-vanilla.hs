{-# LANGUAGE DeriveDataTypeable #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Typeable
import qualified Control.Exception as Exception

data MyException = MyException
  deriving (Show, Typeable)

instance Exception.Exception MyException

task mv = do
  putMVar mv 1
  print "Task done!"

main = do
  mv <- newEmptyMVar
  t <- forkIO $ task mv
  yield
  throwTo t MyException
  v <- takeMVar mv
  print $ "Main done!" ++ (show v)

