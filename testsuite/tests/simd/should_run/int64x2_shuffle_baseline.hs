{-# LANGUAGE MagicHash, UnboxedTuples, ExtendedLiterals #-}
import GHC.Int
import GHC.Prim

test :: (Int, Int) -> (Int64X2# -> Int64X2# -> Int64X2#) -> IO ()
test t shuffle = do
  let a = packInt64X2# (# 0#Int64, 11#Int64 #)
      b = packInt64X2# (# 22#Int64, 33#Int64 #)
      c = shuffle a b
      (# x0, x1 #) = unpackInt64X2# c
  putStrLn $ show t ++ ": " ++ show (I64# x0, I64# x1)
{-# NOINLINE test #-}

test0x :: (Int, Int) -> (Int64X2# -> Int64X2#) -> IO ()
test0x t shuffle = do
  let b = packInt64X2# (# 102#Int64, 103#Int64 #)
      c = shuffle b
      (# x0, x1 #) = unpackInt64X2# c
  putStrLn $ show t ++ ": " ++ show (I64# x0, I64# x1)
{-# NOINLINE test0x #-}

testx0 :: (Int, Int) -> (Int64X2# -> Int64X2#) -> IO ()
testx0 t shuffle = do
  let a = packInt64X2# (# 100#Int64, 101#Int64 #)
      c = shuffle a
      (# x0, x1 #) = unpackInt64X2# c
  putStrLn $ show t ++ ": " ++ show (I64# x0, I64# x1)
{-# NOINLINE testx0 #-}

main :: IO ()
main = do
  test (0, 0) (\a b -> shuffleInt64X2# a b (# 0#, 0# #))
  test (0, 1) (\a b -> shuffleInt64X2# a b (# 0#, 1# #))
  test (0, 2) (\a b -> shuffleInt64X2# a b (# 0#, 2# #))
  test (0, 3) (\a b -> shuffleInt64X2# a b (# 0#, 3# #))
  test (1, 0) (\a b -> shuffleInt64X2# a b (# 1#, 0# #))
  test (1, 1) (\a b -> shuffleInt64X2# a b (# 1#, 1# #))
  test (1, 2) (\a b -> shuffleInt64X2# a b (# 1#, 2# #))
  test (1, 3) (\a b -> shuffleInt64X2# a b (# 1#, 3# #))
  test (2, 0) (\a b -> shuffleInt64X2# a b (# 2#, 0# #))
  test (2, 1) (\a b -> shuffleInt64X2# a b (# 2#, 1# #))
  test (2, 2) (\a b -> shuffleInt64X2# a b (# 2#, 2# #))
  test (2, 3) (\a b -> shuffleInt64X2# a b (# 2#, 3# #))
  test (3, 0) (\a b -> shuffleInt64X2# a b (# 3#, 0# #))
  test (3, 1) (\a b -> shuffleInt64X2# a b (# 3#, 1# #))
  test (3, 2) (\a b -> shuffleInt64X2# a b (# 3#, 2# #))
  test (3, 3) (\a b -> shuffleInt64X2# a b (# 3#, 3# #))

  test0x (0, 0) (\b -> shuffleInt64X2# (broadcastInt64X2# 0#Int64) b (# 0#, 0# #))
  test0x (0, 1) (\b -> shuffleInt64X2# (broadcastInt64X2# 0#Int64) b (# 0#, 1# #))
  test0x (0, 2) (\b -> shuffleInt64X2# (broadcastInt64X2# 0#Int64) b (# 0#, 2# #))
  test0x (0, 3) (\b -> shuffleInt64X2# (broadcastInt64X2# 0#Int64) b (# 0#, 3# #))
  test0x (1, 0) (\b -> shuffleInt64X2# (broadcastInt64X2# 0#Int64) b (# 1#, 0# #))
  test0x (1, 1) (\b -> shuffleInt64X2# (broadcastInt64X2# 0#Int64) b (# 1#, 1# #))
  test0x (1, 2) (\b -> shuffleInt64X2# (broadcastInt64X2# 0#Int64) b (# 1#, 2# #))
  test0x (1, 3) (\b -> shuffleInt64X2# (broadcastInt64X2# 0#Int64) b (# 1#, 3# #))
  test0x (2, 0) (\b -> shuffleInt64X2# (broadcastInt64X2# 0#Int64) b (# 2#, 0# #))
  test0x (2, 1) (\b -> shuffleInt64X2# (broadcastInt64X2# 0#Int64) b (# 2#, 1# #))
  test0x (2, 2) (\b -> shuffleInt64X2# (broadcastInt64X2# 0#Int64) b (# 2#, 2# #))
  test0x (2, 3) (\b -> shuffleInt64X2# (broadcastInt64X2# 0#Int64) b (# 2#, 3# #))
  test0x (3, 0) (\b -> shuffleInt64X2# (broadcastInt64X2# 0#Int64) b (# 3#, 0# #))
  test0x (3, 1) (\b -> shuffleInt64X2# (broadcastInt64X2# 0#Int64) b (# 3#, 1# #))
  test0x (3, 2) (\b -> shuffleInt64X2# (broadcastInt64X2# 0#Int64) b (# 3#, 2# #))
  test0x (3, 3) (\b -> shuffleInt64X2# (broadcastInt64X2# 0#Int64) b (# 3#, 3# #))

  testx0 (0, 0) (\a -> shuffleInt64X2# a (broadcastInt64X2# 0#Int64) (# 0#, 0# #))
  testx0 (0, 1) (\a -> shuffleInt64X2# a (broadcastInt64X2# 0#Int64) (# 0#, 1# #))
  testx0 (0, 2) (\a -> shuffleInt64X2# a (broadcastInt64X2# 0#Int64) (# 0#, 2# #))
  testx0 (0, 3) (\a -> shuffleInt64X2# a (broadcastInt64X2# 0#Int64) (# 0#, 3# #))
  testx0 (1, 0) (\a -> shuffleInt64X2# a (broadcastInt64X2# 0#Int64) (# 1#, 0# #))
  testx0 (1, 1) (\a -> shuffleInt64X2# a (broadcastInt64X2# 0#Int64) (# 1#, 1# #))
  testx0 (1, 2) (\a -> shuffleInt64X2# a (broadcastInt64X2# 0#Int64) (# 1#, 2# #))
  testx0 (1, 3) (\a -> shuffleInt64X2# a (broadcastInt64X2# 0#Int64) (# 1#, 3# #))
  testx0 (2, 0) (\a -> shuffleInt64X2# a (broadcastInt64X2# 0#Int64) (# 2#, 0# #))
  testx0 (2, 1) (\a -> shuffleInt64X2# a (broadcastInt64X2# 0#Int64) (# 2#, 1# #))
  testx0 (2, 2) (\a -> shuffleInt64X2# a (broadcastInt64X2# 0#Int64) (# 2#, 2# #))
  testx0 (2, 3) (\a -> shuffleInt64X2# a (broadcastInt64X2# 0#Int64) (# 2#, 3# #))
  testx0 (3, 0) (\a -> shuffleInt64X2# a (broadcastInt64X2# 0#Int64) (# 3#, 0# #))
  testx0 (3, 1) (\a -> shuffleInt64X2# a (broadcastInt64X2# 0#Int64) (# 3#, 1# #))
  testx0 (3, 2) (\a -> shuffleInt64X2# a (broadcastInt64X2# 0#Int64) (# 3#, 2# #))
  testx0 (3, 3) (\a -> shuffleInt64X2# a (broadcastInt64X2# 0#Int64) (# 3#, 3# #))
