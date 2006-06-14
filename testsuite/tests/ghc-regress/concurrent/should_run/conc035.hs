module Main where

import Control.Concurrent
import qualified Control.Exception as E

trapHandler :: MVar Int -> MVar () -> IO ()
trapHandler inVar caughtVar =
  (do E.block $ do
          trapMsg <- takeMVar inVar
          putStrLn ("Handler got: " ++ show trapMsg)
      trapHandler inVar caughtVar
  )
  `E.catch`
  (trapExc inVar caughtVar)

trapExc :: MVar Int -> MVar () -> E.Exception -> IO ()
trapExc inVar caughtVar e =
  do putStrLn ("Exception: " ++ show e)
     putMVar caughtVar ()
     trapHandler inVar caughtVar

main :: IO ()
main = do
  inVar <- newEmptyMVar
  caughtVar <- newEmptyMVar
  tid <- forkIO (trapHandler inVar caughtVar)
  yield
  putMVar inVar 1
  threadDelay 1000
  throwTo tid (E.ErrorCall "1st")
  takeMVar caughtVar
  putMVar inVar 2
  threadDelay 1000
  throwTo tid (E.ErrorCall "2nd")
	-- the second time around, exceptions will be blocked, because
	-- the trapHandler is effectively "still in the handler" from the
	-- first exception.  I'm not sure if this is by design or by
	-- accident.  Anyway, the trapHandler will at some point block
	-- in takeMVar, and thereby become interruptible, at which point
	-- it will receive the second exception.
  takeMVar caughtVar
  putStrLn "All done"
