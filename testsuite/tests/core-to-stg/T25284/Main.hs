{-

  This tests that speculative evaluation for dictionary functions works as
  expected, with a large dictionary that goes unused.

   - Module A: dictfun speculative evaluation enabled
   - Module B: dictfun speculative evaluation disabled

  Speculative evaluation causes the unused large dictionary to be allocated
  strictly in module A, so we expect more allocations than in module B.

 -}
module Main where

import qualified A
import qualified B
import qualified Cls

import Data.Word
import System.Mem (performGC)
import GHC.Stats
import Control.Monad

{-# NOINLINE getAllocated #-}
getAllocated :: IO Word64
getAllocated = do
  performGC
  allocated_bytes <$> getRTSStats

main :: IO ()
main = do
    -- warm up (just in case)
    _       <- testMain A.testX
    _       <- testMain B.testX

    -- for real
    a_alloc <- testMain A.testX
    b_alloc <- testMain B.testX

    -- expect B to allocate less than A
    let alloc_ratio :: Double
        alloc_ratio = fromIntegral b_alloc / fromIntegral a_alloc
    putStrLn ("expected alloc: " ++ show (alloc_ratio < 0.7))

iter :: (Int -> IO ()) -> Int -> Int -> IO ()
iter m !i !j
  | i < j = m i >> iter m (i+1) j
  | otherwise = pure ()

{-# NOINLINE testMain #-}
testMain :: (forall b. (Show b, Cls.HasConst b) => b -> Int -> IO ())
         -> IO Word64
testMain f = do
  alloc0 <- getAllocated
  iter (\i -> f (0::Int) i >> f (0::Word) i) 1 100000
  alloc1 <- getAllocated
  pure (alloc1 - alloc0)
