{-# OPTIONS -XImpredicativeTypes -fno-warn-deprecated-flags #-}

module ShouldFail where

import Control.Concurrent

-- Attempt to put a polymorphic value in an MVar
-- Fails, but the error message is worth keeping an eye on
--
-- Actually (Dec 06) it succeeds now
--
-- In GHC 7.0 it fails again! (and rightly so)

foo = do var <- newEmptyMVar :: IO (MVar (forall a. Show a => a -> String))
         putMVar var (show :: forall b. Show b => b -> String)

