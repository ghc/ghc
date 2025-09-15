-- Tests for a bug fixed in

module Main where

import System.Environment
import Control.Concurrent
import Control.Monad

foreign import ccall safe "out"
  out :: Int -> IO Int

foreign export ccall "incall"  incall :: Int -> IO Int

incall :: Int -> IO Int
incall x = return $ x + 1

main = do
  [n, m] <- fmap (fmap read) getArgs
  ms <- replicateM m $ do
    v <- newEmptyMVar
    forkIO $ do mapM out [0..n]; putMVar v ()
    return v
  mapM_ takeMVar ms
