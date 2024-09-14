{-# LANGUAGE MagicHash, UnboxedTuples #-}

module T23847 where


import GHC.Internal.IO.Magic
import GHC.IO
import Data.Coerce
import Data.Kind

considerExceptionsPrecise :: forall (a :: Type). IO a -> IO a
considerExceptionsPrecise = coerce (considerExceptionsPrecise# @a)

evalBarrier :: IO ()
evalBarrier = IO (\s -> (# evalBarrier# s, () #))

hideEvalBarriers :: forall (a :: Type). IO a -> IO a
hideEvalBarriers = coerce (hideEvalBarriers# @a)

testFun1 :: Int -> Int -> IO ()
{-# OPAQUE testFun1 #-}
testFun1 x y = do
  evaluate x
  evaluate y
  print $! x + y

testFun2 :: Int -> Int -> IO ()
{-# OPAQUE testFun2 #-}
testFun2 x y = do
-- The strictness of this function in x and y
-- should be masked by the considerExceptionsPrecise call.
  considerExceptionsPrecise (evaluate x)
  evaluate y
  print $! x + y

testFun3 :: Int -> Int -> IO ()
{-# OPAQUE testFun3 #-}
-- The strictness of this function in x
-- should be masked by the considerExceptionsPrecise call.
-- But it remains strict in y.
testFun3 x y = do
  evaluate x
  considerExceptionsPrecise (evaluate $! y)
  print $! x + y

testFun4 :: Int -> Int -> IO ()
{-# OPAQUE testFun4 #-}
testFun4 x y = do
  evaluate $! x
  evalBarrier
  evaluate y
  print $! x + y

testFun5 :: Int -> Int -> IO ()
{-# OPAQUE testFun5 #-}
testFun5 x y = do
  hideEvalBarriers $ do
    evaluate x
    evalBarrier
    evaluate y
  print $! x + y

testFun6 :: Int -> Int -> Int -> IO ()
{-# OPAQUE testFun6 #-}
-- This function should be strict in x and z, but not y
testFun6 x y z = do
  hideEvalBarriers $ do
    considerExceptionsPrecise (evaluate $! x)
    evaluate $! y
  print $! x + y + z
