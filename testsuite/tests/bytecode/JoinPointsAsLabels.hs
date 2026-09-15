{-# LANGUAGE MagicHash #-}
-- Join points whose jumps all stay in the BCO of their definition are
-- compiled as labels, see Note [Join points as labels]. The functions cover
-- parameters of various representations, jumps from above the join point's
-- base depth, nested join points, void parameters, and RHSs that keep their
-- parameters alive across a GC.
module Main where

import GHC.Exts
import GHC.Int
import GHC.Word
import System.Mem (performGC)

-- Parameters of different sizes and representations, jumps from both branches
-- of an inlined case. (The join points in this module have free variables, so
-- that they are not floated out.)
mixed :: Int# -> Int# -> Int
mixed x y =
  let j :: Int# -> Double# -> Word8# -> Int64# -> Int -> Int
      j a d w i l = I# (a *# x) + truncate (D# d) + fromIntegral (W8# w)
                    + fromIntegral (I64# i) + l
      {-# NOINLINE j #-}
  in if isTrue# (x <# y)
       then j x 1.5## (wordToWord8# 200##) (intToInt64# 7#) (I# y)
       else j y 2.5## (wordToWord8# 3##) (intToInt64# (0# -# 9#)) (I# x)
{-# NOINLINE mixed #-}

-- Jumps from alternatives with binders, i.e. from above the base depth.
deep :: Int# -> Int
deep x =
  let j :: Int# -> Int# -> Int
      j a b = I# (a *# 1000# +# b -# x)
      {-# NOINLINE j #-}
  in case x +# 1# of
       r -> case r *# r of
              0# -> j r 1#
              s -> case s -# x of
                     t -> j t s
{-# NOINLINE deep #-}

-- The inner join point jumps to the outer one.
nest :: Int# -> Int# -> Int
nest x y =
  let outer :: Int# -> Int
      outer a = I# (a +# y +# 100#)
      {-# NOINLINE outer #-}
  in let inner :: Int# -> Int
         inner b = if isTrue# (b ># 10#) then outer b else outer (b +# x)
         {-# NOINLINE inner #-}
     in case x of
          0# -> inner y
          1# -> inner (y +# 1#)
          _  -> outer x
{-# NOINLINE nest #-}

-- A lifted parameter stays on the stack while the RHS evaluates a thunk that
-- allocates and collects garbage.
gcAcross :: Int -> Int# -> Int
gcAcross n x =
  let j :: [Int] -> Int# -> Int
      j xs k = case length (show (sum [1 .. n])) of
                 len -> len + I# k + sum xs
      {-# NOINLINE j #-}
  in if isTrue# (x ># 0#) then j [1, 2, 3] x else j [I# x, 5] 2#
{-# NOINLINE gcAcross #-}

-- IO code: join points with a State# parameter.
io :: Int -> IO Int
io n = do
  r <- if n > 3 then pure (n * 2) else pure (n + 1)
  performGC
  if even r then pure (r + 10) else pure (r - 10)
{-# NOINLINE io #-}

main :: IO ()
main = do
  print (mixed 3# 4#, mixed 5# 4#)
  print (deep 0#, deep (0# -# 1#), deep 5#)
  print (nest 0# 5#, nest 1# 20#, nest 7# 1#)
  print (gcAcross 100000 3#, gcAcross 200000 (0# -# 1#))
  mapM io [1 .. 6] >>= print
