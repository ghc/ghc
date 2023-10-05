module Main where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

chkLoop :: TVar String -> STM ()
chkLoop v = do
  val <- readTVar v
  if (length val == 2) then retry else return ()

main :: IO ()
main = do
  v <- newTVarIO "hi"
  atomically $ do
    chkLoop v `orElse` return ()
    error "you're expected to see this"
