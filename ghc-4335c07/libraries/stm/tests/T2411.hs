{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Main where
import Control.Concurrent.STM
import GHC.Conc
import Control.Exception
import Data.List

main = do
  tv <- atomically $ newTVar "test"
  mapM_ (go tv) (map show ([1..100] ++ [1..1000] :: [Int]))
        where go tv s = forkIO $ do
                          x <- atomically $ (do
                                   writeTVar tv "testing"
                                   if read s `mod` 25 == 0
                                     then throw $ AssertionFailed ("SimulatedException " ++ s)
                                     else return s) `catchSTM` (\e -> return (show (e::SomeException)))
                          putStrLn x
