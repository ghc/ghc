{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import GHC.Exts
import qualified GHC.Internal.Unboxed.Do as Ubx
import GHC.Internal.Unboxed.Do (ST#)

fill :: MutableByteArray# s -> Word# -> State# s -> State# s -- unit result
fill mba x = Ubx.do
  writeWordArray# mba 0# x             -- unit action as a statement
  n <- getSizeofMutableByteArray# mba  -- binds an Int#
  writeWordArray# mba 1# (int2Word# n)

sumAll :: Int# -> ST# s Word#
sumAll n = Ubx.do
  mba <- newByteArray# n                  -- binds an unlifted pointer
  fill mba 40##
  a <- readWordArray# mba 0#              -- binds a Word#
  b <- readWordArray# mba 1#
  (# c, ov #) <- Ubx.pure (addWordC# a b) -- binds an unboxed tuple
  let !(# hi, lo #) = timesWord2# a 2##
  Ubx.pure (plusWord# (plusWord# c (int2Word# ov)) (plusWord# hi lo))

ordering :: MutableByteArray# s -> ST# s Ordering -- lifted result
ordering mba = Ubx.do
  a <- readWordArray# mba 0#
  b <- readWordArray# mba 1#
  if | isTrue# (ltWord# a b) -> Ubx.pure LT
     | isTrue# (gtWord# a b) -> Ubx.pure GT
     | True                  -> Ubx.pure EQ

main :: IO ()
main = do
  print (W# (case runRW# (sumAll 16#) of (# _, w #) -> w))
  print (case runRW# (\s -> case newByteArray# 16# s of
                        (# s1, mba #) -> case fill mba 5## s1 of
                          s2 -> ordering mba s2) of
           (# _, o #) -> o)
