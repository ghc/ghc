{-# LANGUAGE MagicHash, UnboxedTuples #-}

import GHC.Prim
import GHC.Word
import GHC.Exts

import Control.Monad
import Data.Bits
import Data.List
import System.Exit

allEqual :: Eq a => [a] -> Bool
allEqual [] = error "allEqual: nothing to compare"
allEqual (x:xs) = all (== x) xs

testWords :: [Word]
testWords = map head . group . sort $
            concatMap (\w -> [w - 1, w, w + 1]) $
            concatMap (\w -> [w, maxBound - w]) $
            trailingOnes ++ randoms
  where trailingOnes = takeWhile (/= 0) $ iterate (`div` 2) $ maxBound
        -- What would a Haskell program be without some Fibonacci numbers?
        randoms = take 40 $ drop 100 fibs
        fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


wordSizeInBits :: Int
wordSizeInBits = length $ takeWhile (/= 0) $ iterate (`div` 2) (maxBound :: Word)


-- plusWord2# (Word# carry)

ways_plusWord2# :: [Word -> Word -> Bool]
ways_plusWord2# = [ltTest, integerTest, primopTest]
  where ltTest x y =
          let r = x + y in r < x
        integerTest x y =
          let r = fromIntegral x + fromIntegral y :: Integer
          in r > fromIntegral (maxBound :: Word)
        primopTest (W# x) (W# y) = case plusWord2# x y of
          (# 0##, _ #) -> False
          (# 1##, _ #) -> True
          _            -> error "unexpected result from plusWord2#"

-- addIntC# (Int# addition overflow)

ways_addIntC# :: [Int -> Int -> Bool]
ways_addIntC# = [ltTest, integerTest, highBitTest, primopTest]
  where ltTest x y =
          let r = x + y in (y > 0 && r < x) || (y < 0 && r > x)
        integerTest x y =
          let r = fromIntegral x + fromIntegral y :: Integer
          in r < fromIntegral (minBound :: Int) || r > fromIntegral (maxBound :: Int)
        highBitTest x y =
          let r = x + y in testBit ((x `xor` r) .&. (y `xor` r)) (wordSizeInBits - 1)
        primopTest (I# x) (I# y) = case addIntC# x y of
          (# _, 0# #) -> False
          _ -> True

-- subIntC# (Int# subtraction overflow)

ways_subIntC# :: [Int -> Int -> Bool]
ways_subIntC# = [ltTest, integerTest, highBitTest, primopTest]
  where ltTest x y =
          let r = x - y in (y > 0 && r > x) || (y < 0 && r < x)
        integerTest x y =
          let r = fromIntegral x - fromIntegral y :: Integer
          in r < fromIntegral (minBound :: Int) || r > fromIntegral (maxBound :: Int)
        highBitTest x y =
          let r = x - y in testBit ((x `xor` r) .&. complement (y `xor` r)) (wordSizeInBits - 1)
        primopTest (I# x) (I# y) = case subIntC# x y of
          (# _, 0# #) -> False
          _ -> True

runTest :: Show a => String -> [a -> a -> Bool] -> a -> a -> IO ()
runTest label ways x y = do
  let results = map (\f -> f x y) ways
  unless (allEqual results) $ do
    putStrLn $ "Failed (" ++ label ++ "): " ++ show (x,y) ++ " " ++ show results
    exitWith (ExitFailure 1)

main :: IO ()
main = do
  forM_ testWords $ \x ->
    forM_ testWords $ \y -> do
      runTest "ways_plusWord2#" ways_plusWord2# x y
      runTest "ways_addIntC#" ways_addIntC# (fromIntegral x) (fromIntegral y)
      runTest "ways_subIntC#" ways_subIntC# (fromIntegral x) (fromIntegral y)
  putStrLn "Passed"
