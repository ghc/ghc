{-# LANGUAGE MagicHash, UnboxedTuples #-}
import GHC.Prim
import GHC.Exts

test :: (Int, Int) -> (DoubleX2# -> DoubleX2# -> DoubleX2#) -> IO ()
test t@(i0, i1) shuffle = do
  let a = packDoubleX2# (# 0.0##, 1.1## #)
      b = packDoubleX2# (# 2.2##, 3.3## #)
      c = shuffle a b
      (# x0, x1 #) = unpackDoubleX2# c
      ok = if map ([0.0, 1.1, 2.2, 3.3] !!) [i0, i1] == [D# x0, D# x1]
           then ""
           else "...WRONG"
  putStrLn $ show t ++ ": " ++ show (D# x0, D# x1) ++ ok
{-# NOINLINE test #-}

testFlipped :: (Int, Int) -> (DoubleX2# -> DoubleX2# -> DoubleX2#) -> IO ()
testFlipped t@(i0, i1) shuffle = do
  let a = packDoubleX2# (# 0.0##, 1.1## #)
      b = packDoubleX2# (# 2.2##, 3.3## #)
      c = shuffle a b
      (# x0, x1 #) = unpackDoubleX2# c
      ok = if map ([2.2, 3.3, 0.0, 1.1] !!) [i0, i1] == [D# x0, D# x1]
           then ""
           else "...WRONG"
  putStrLn $ show t ++ ": " ++ show (D# x0, D# x1) ++ ok
{-# NOINLINE testFlipped #-}

main :: IO ()
main = do
  test (0, 0) (\a b -> shuffleDoubleX2# a b (# 0#, 0# #))
  test (0, 1) (\a b -> shuffleDoubleX2# a b (# 0#, 1# #))
  test (0, 2) (\a b -> shuffleDoubleX2# a b (# 0#, 2# #))
  test (0, 3) (\a b -> shuffleDoubleX2# a b (# 0#, 3# #))
  test (1, 0) (\a b -> shuffleDoubleX2# a b (# 1#, 0# #))
  test (1, 1) (\a b -> shuffleDoubleX2# a b (# 1#, 1# #))
  test (1, 2) (\a b -> shuffleDoubleX2# a b (# 1#, 2# #))
  test (1, 3) (\a b -> shuffleDoubleX2# a b (# 1#, 3# #))
  test (2, 0) (\a b -> shuffleDoubleX2# a b (# 2#, 0# #))
  test (2, 1) (\a b -> shuffleDoubleX2# a b (# 2#, 1# #))
  test (2, 2) (\a b -> shuffleDoubleX2# a b (# 2#, 2# #))
  test (2, 3) (\a b -> shuffleDoubleX2# a b (# 2#, 3# #))
  test (3, 0) (\a b -> shuffleDoubleX2# a b (# 3#, 0# #))
  test (3, 1) (\a b -> shuffleDoubleX2# a b (# 3#, 1# #))
  test (3, 2) (\a b -> shuffleDoubleX2# a b (# 3#, 2# #))
  test (3, 3) (\a b -> shuffleDoubleX2# a b (# 3#, 3# #))

  testFlipped (0, 0) (\a b -> shuffleDoubleX2# b a (# 0#, 0# #))
  testFlipped (0, 1) (\a b -> shuffleDoubleX2# b a (# 0#, 1# #))
  testFlipped (0, 2) (\a b -> shuffleDoubleX2# b a (# 0#, 2# #))
  testFlipped (0, 3) (\a b -> shuffleDoubleX2# b a (# 0#, 3# #))
  testFlipped (1, 0) (\a b -> shuffleDoubleX2# b a (# 1#, 0# #))
  testFlipped (1, 1) (\a b -> shuffleDoubleX2# b a (# 1#, 1# #))
  testFlipped (1, 2) (\a b -> shuffleDoubleX2# b a (# 1#, 2# #))
  testFlipped (1, 3) (\a b -> shuffleDoubleX2# b a (# 1#, 3# #))
  testFlipped (2, 0) (\a b -> shuffleDoubleX2# b a (# 2#, 0# #))
  testFlipped (2, 1) (\a b -> shuffleDoubleX2# b a (# 2#, 1# #))
  testFlipped (2, 2) (\a b -> shuffleDoubleX2# b a (# 2#, 2# #))
  testFlipped (2, 3) (\a b -> shuffleDoubleX2# b a (# 2#, 3# #))
  testFlipped (3, 0) (\a b -> shuffleDoubleX2# b a (# 3#, 0# #))
  testFlipped (3, 1) (\a b -> shuffleDoubleX2# b a (# 3#, 1# #))
  testFlipped (3, 2) (\a b -> shuffleDoubleX2# b a (# 3#, 2# #))
  testFlipped (3, 3) (\a b -> shuffleDoubleX2# b a (# 3#, 3# #))
