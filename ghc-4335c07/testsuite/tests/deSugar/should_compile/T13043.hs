{-# LANGUAGE BangPatterns #-}
module T13043 (foo, bar) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE scServerState #-}
scServerState :: SCServerState
scServerState = unsafePerformIO (return undefined)

data SCServerState = SCServerState
     { scServer_socket :: IORef (Maybe Int)
     }

foo :: IO Int
foo = do
   let !_ = scServerState
   readIORef (scServer_socket scServerState) >>= \xs -> case xs of
      Nothing -> do
         s <- undefined
         writeIORef (scServer_socket scServerState) (Just s)
         return s
      Just s -> return s

bar :: IO ()
bar = do
   let !_ = scServerState
   return ()
