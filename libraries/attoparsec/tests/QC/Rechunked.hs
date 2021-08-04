{-# LANGUAGE BangPatterns #-}

module QC.Rechunked (
      rechunkBS
    , rechunkT
    ) where

import Control.Monad (forM, forM_)
import Control.Monad.ST (ST, runST)
import Data.List (unfoldr)
import Test.QuickCheck (Gen, choose)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M

rechunkBS :: B.ByteString -> Gen [B.ByteString]
rechunkBS = fmap (map B.copy) . rechunk_ B.splitAt B.length

rechunkT :: T.Text -> Gen [T.Text]
rechunkT = fmap (map T.copy) . rechunk_ T.splitAt T.length

rechunk_ :: (Int -> a -> (a,a)) -> (a -> Int) -> a -> Gen [a]
rechunk_ split len xs = (unfoldr go . (,) xs) `fmap` rechunkSizes (len xs)
  where go (b,r:rs)   = Just (h, (t,rs))
          where (h,t) = split r b
        go (_,_)      = Nothing

rechunkSizes :: Int -> Gen [Int]
rechunkSizes n0 = shuffle =<< loop [] (0:repeat 1) n0
  where loop _ [] _ = error "it's 2015, where's my Stream type?"
        loop acc (lb:lbs) n
          | n <= 0 = shuffle (reverse acc)
          | otherwise = do
            !i <- choose (lb,n)
            loop (i:acc) lbs (n-i)

shuffle :: [Int] -> Gen [Int]
shuffle (0:xs) = (0:) `fmap` shuffle xs
shuffle xs = fisherYates xs

fisherYates :: [a] -> Gen [a]
fisherYates xs = (V.toList . V.backpermute v) `fmap` swapIndices (G.length v)
  where
    v = V.fromList xs
    swapIndices n0 = do
        swaps <- forM [0..n] $ \i -> ((,) i) `fmap` choose (i,n)
        return (runST (swapAll swaps))
      where
        n = n0 - 1
        swapAll :: [(Int,Int)] -> ST s (V.Vector Int)
        swapAll ijs = do
          mv <- G.unsafeThaw (G.enumFromTo 0 n :: V.Vector Int)
          forM_ ijs $ uncurry (M.swap mv)
          G.unsafeFreeze mv
