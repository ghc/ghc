{-# OPTIONS_GHC -Wall  #-}
-- based on https://byorgey.github.io/blog/posts/2024/06/21/cpih-product-divisors.html


import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Foldable

-- This repro code turned out to be delicate wrt integer overflow
-- See comments in #25055
-- So, for reproducibility we use Int32, to make sure the code works on
--    32 bit machines with no overflow issues
import GHC.Int

smallest :: Int32 -> UArray Int32 Int32
smallest maxN = runSTUArray $ do
  arr <- newGenArray (2,maxN) initA
  for_ [5, 7 .. maxN] $ \k -> do
      k' <- readArray arr k
      when (k == k') $ do
        -- for type Int32 when k = 46349, k * k is negative
        -- for_ [k*k, k*(k+2) .. maxN] $ \oddMultipleOfK -> do
        for_ [k, k + 2 .. maxN] $ \oddMultipleOfK -> do
          modifyArray' arr oddMultipleOfK (min k)
  return arr
    where
      initA i
        | even i          = return 2
        | i `rem` 3 == 0  = return 3
        | otherwise       = return i

factor :: STUArray s Int32 Int32 -> Int32 -> Int32 -> ST s ()
-- With #25055 the program ran slow as it appear below, but
-- fast if you (a) comment out 'let p = smallest maxN ! m'
--             (b) un-comment the commented-out bindings for p and sm
factor countsArr maxN n  = go n
  where
    -- sm = smallest maxN

    go 1 = return ()
    go m = do
      -- let p = sm ! m
      let p = smallest maxN ! m
      modifyArray' countsArr p (+1)
      go (m `div` p)


counts :: Int32 -> [Int32] ->  UArray Int32 Int32
counts maxN ns  = runSTUArray $ do
  cs <- newArray (2,maxN) 0
  for_ ns (factor cs maxN)
  return cs

solve :: [Int32] -> Int32
solve = product . map (+ 1) . elems . counts 1000000

main :: IO ()
main =
  -- print $ maximum $ elems $ smallest 1000000
  print $ solve [1..100]
