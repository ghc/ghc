{-# LANGUAGE MagicHash, UnboxedTuples #-}
-- Compiled with -fno-float-in: otherwise the join points below would be
-- floated into the single case alternative that uses them.
--
-- Join points whose jumps are all in one case continuation BCO are compiled
-- as labels in that BCO, see Note [Join points as labels]. The scrutinees
-- call functions, so the alternatives of these cases go into continuation
-- BCOs.
module Main where

import GHC.Exts
import GHC.IO (IO (..))
import System.Mem (performGC)

opaque :: Int -> Int
opaque x = x * 3 - 1
{-# NOINLINE opaque #-}

data T = A Int# Int# | B Int#

opaqueT :: Int -> T
opaqueT (I# x)
  | isTrue# (x ># 10#) = B (x -# 10#)
  | otherwise = A x (x *# 2#)
{-# NOINLINE opaqueT #-}

-- All jumps in one continuation.
cont1 :: Int -> Int -> Int
cont1 n x =
  let j :: Int# -> Int
      j a = I# (a +# 1#) + n
      {-# NOINLINE j #-}
  in case opaque x of
       I# 2# -> j 40#
       I# y  -> j y
{-# NOINLINE cont1 #-}

-- All jumps in a continuation nested in another one.
cont2 :: Int -> Int -> Int
cont2 n x =
  let j :: Int# -> Int -> Int
      j a b = I# a * 100 + b + n
      {-# NOINLINE j #-}
  in case opaque x of
       I# 2# -> n
       I# y  -> case opaque (I# y) of
                  I# 5# -> j y 7
                  z     -> j 3# z
{-# NOINLINE cont2 #-}

-- Jumps from UNPACKed alternatives and from an inlined case inside the
-- continuation.
contUnpack :: Int -> Int -> Int
contUnpack n x =
  let j :: Int# -> Int
      j a = I# (a *# 2#) - n
      {-# NOINLINE j #-}
  in case opaqueT x of
       A a b -> j (a +# b)
       B c   -> case c of
                  7# -> j 1000#
                  r  -> if isTrue# (r ># 50#) then j (r -# 50#) else j r
{-# NOINLINE contUnpack #-}

-- The scrutinee collects garbage while a list that only the RHS of the join
-- point uses is on the stack of the defining BCO.
gcScrut :: [Int] -> IO Int
gcScrut xs = IO (\s0 ->
  let j :: Int# -> State# RealWorld -> (# State# RealWorld, Int #)
      j k s1 = (# s1, I# k + sum xs #)
      {-# NOINLINE j #-}
  in case collect s0 of
       (# s2, 0# #) -> j 1# s2
       (# s2, k #)  -> j k s2)
{-# NOINLINE gcScrut #-}

collect :: State# RealWorld -> (# State# RealWorld, Int# #)
collect s = case performGC of IO m -> case m s of (# s', () #) -> (# s', 5# #)
{-# NOINLINE collect #-}

main :: IO ()
main = do
  print (map (cont1 10) [1, 3], map (cont2 10) [1, 2, 4])
  print (map (contUnpack 10) [2, 17, 71, 80])
  mapM (\k -> gcScrut [k .. k + 1000]) [1, 2] >>= print
