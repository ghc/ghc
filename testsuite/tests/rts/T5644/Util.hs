{-# LANGUAGE BangPatterns #-}

module Util where

import Data.Time
-- import Data.List.Split (splitEvery)

import Conf

timed act = do
  putStrLn ""
  t0 <- getCurrentTime
  !v <- act
  t1 <- getCurrentTime
  let td = diffUTCTime t1 t0
  putStrLn $ "Action time: " ++ show td
  return (v,td)

splitEvery _ [] = []
splitEvery n xs = let (lxs,rxs) = splitAt n xs in lxs : splitEvery n rxs

runTest :: (IO ()) -> IO ()
runTest test = do
  (_, t) <- timed test
  let format x = unwords . reverse . map reverse . splitEvery 3 . reverse . show $ x
      val = format (round (fromIntegral iTERATIONS / realToFrac t :: Double) :: Integer)

  putStr "OpsPerSecond: " 
  putStrLn val