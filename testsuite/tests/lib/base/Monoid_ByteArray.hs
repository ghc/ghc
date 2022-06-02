{-# LANGUAGE DerivingVia, TypeApplications, OverloadedLists #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Array.Byte
import Data.Bits
import Data.Either
import Data.List (tails)
import Data.Ord
import Data.Semigroup
import GHC.Exts (IsList(..))


newtype Tricky = Tricky Int
  deriving (Eq, Num, Real, Enum, Integral) via Int
  deriving Ord via Down Int

smallArrs :: [ByteArray]
-- 40 arrays, of total length 600
smallArrs = [[], [3,8,1], [0], [255], [1,6,1,8,2]] ++
  map fromList (replicate 29 0 : tails [0..32])

shouldError :: a -> IO () -> IO ()
shouldError val ifNoError = do
  res <- try @ErrorCall (evaluate val)
  when (isRight res) ifNoError

testConcat :: [ByteArray] -> IO ()
testConcat arrs = do
  let lis = map toList arrs
      expected = mconcat lis
      actual = toList (mconcat arrs)
  when (expected /= actual) $
    putStrLn $ unwords ["mconcat", show lis, "/=", show actual]

main :: IO ()
main = do
  when (toList @ByteArray mempty /= []) $
    putStrLn "mempty /= []"

  -- test <>
  forM_ smallArrs $ \x -> do
    let xli = toList x
    forM_ smallArrs $ \y -> do
      let yli = toList y
          expected = xli <> yli
          actual   = toList (x <> y)
      when (expected /= actual) $
        putStrLn $ unwords [show xli, "<>", show yli, "/=", show actual]

  -- test stimes
  forM_ smallArrs $ \x -> do
    let xli = toList x
    shouldError (stimes (-1 :: Integer) x) $
      putStrLn $ unwords ["stimes (-1 :: Integer)", show xli, "didn't fail??"]
    shouldError (stimes (-1 :: Tricky) x) $
      putStrLn $ unwords ["stimes (-1 :: Tricky)",  show xli, "didn't fail??"]
    when (length xli > 1) $ shouldError (stimes (maxBound @Int) x) $
      putStrLn $ unwords ["stimes (maxBound @Int)", show xli, "didn't fail??"]
    forM_ (10000 : [0 :: Int .. 32]) $ \n -> do
      let expected = stimes n xli
          actual   = toList (stimes n x)
      when (expected /= actual) $
        putStrLn $ unwords ["stimes", show n, show xli, "/=", show actual]
  evaluate $ stimes @ByteArray @Int maxBound []
  evaluate $ stimes @ByteArray @Integer (10^100) []

  -- test mconcat
  testConcat []
  forM_ smallArrs $ \x -> do
    testConcat [x]
    forM_ smallArrs $ \y -> do
      testConcat [x, y]
      forM_ smallArrs $ \z -> do -- OK, 40^3 = 64K
        testConcat [x, y, z]

  -- test mconcat's overflow-handling
  let  bigArr = stimes (bit 18 :: Int) [0] :: ByteArray
  when (finiteBitSize @Int 0 == 32) $
    shouldError (mconcat $ replicate (bit 14) bigArr) $
      putStrLn "Impossible mconcat succeeded???"
