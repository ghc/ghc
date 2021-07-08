{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IORef where

import GHC.Base

data IORef a

newIORef :: a -> IO (IORef a)
readIORef :: IORef a -> IO a
writeIORef :: IORef a -> a -> IO ()
