module Main where

import Control.Exception
import Control.Monad
import System.Mem

import Control.Monad.ST
import Data.Array
import Data.Array.ST
import qualified Data.Array.Unboxed as U
import Control.DeepSeq

import Data.Compact

assertFail :: String -> IO ()
assertFail msg = throwIO $ AssertionFailed msg

assertEquals :: (Eq a, Show a) => a -> a -> IO ()
assertEquals expected actual =
  if expected == actual then return ()
  else assertFail $ "expected " ++ (show expected)
       ++ ", got " ++ (show actual)

arrTest :: (Monad m, MArray a e m, Num e) => m (a Int e)
arrTest = do
  arr <- newArray (1, 10) 0
  forM_ [1..10] $ \j -> do
    writeArray arr j (fromIntegral $ 2*j + 1)
  return arr

instance NFData (U.UArray i e) where
  rnf x = seq x ()

-- test :: (Word -> a -> IO (Maybe (Compact a))) -> IO ()
test func = do
  let fromList :: Array Int Int
      fromList = listArray (1, 10) [1..]
      frozen :: Array Int Int
      frozen = runST $ do
        arr <- arrTest :: ST s (STArray s Int Int)
        freeze arr
      stFrozen :: Array Int Int
      stFrozen = runSTArray arrTest
      unboxedFrozen :: U.UArray Int Int
      unboxedFrozen = runSTUArray arrTest

  let val = (fromList, frozen, stFrozen, unboxedFrozen)
  str <- func 4096 val

  -- check that val is still good
  assertEquals (fromList, frozen, stFrozen, unboxedFrozen) val
  -- check the value in the compact
  assertEquals val (getCompact str)
  performMajorGC
  -- check again the value in the compact
  assertEquals val (getCompact str)

main = do
  test newCompact
  test newCompactNoShare
