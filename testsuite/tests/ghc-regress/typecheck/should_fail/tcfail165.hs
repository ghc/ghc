{-# OPTIONS -fglasgow-exts #-}

module ShouldFail where

import Control.Concurrent

-- Attempt to put a polymorphic value in an MVar
-- Fails, but the error message is worth keeping an eye on

foo = do var <- newEmptyMVar :: IO (MVar (forall a. Show a => a -> String))
         putMVar var (show :: forall a. Show a => a -> String)

