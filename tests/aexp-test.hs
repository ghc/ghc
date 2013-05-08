{-# LANGUAGE DeriveDataTypeable #-}

import LwConc.RunQueue
import LwConc.MVar
import Data.Typeable
import qualified Control.Exception as Exception

data MyException = MyException
  deriving (Show, Typeable)

instance Exception.Exception MyException

task mv = do
  takeMVar mv
  print "Task done!"

main = do
  newSched
  mv <- newEmptyMVar
  t <- forkIO $ task mv
  throwTo t MyException
  yield
  print "Main done!"
