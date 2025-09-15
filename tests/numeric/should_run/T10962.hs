{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Main (main) where

import GHC.Base

unW# :: Word -> Word#
unW# (W# w) = w

type WordOpC = Word# -> Word# -> (# Word#, Int# #)

check :: WordOpC -> Word# -> Word# -> IO ()
check op a b = do
  let (# w, c #) = op a b
  print (W# w, I# c)

checkSubInlNoInl :: WordOpC -> Word# -> Word# -> IO ()
checkSubInlNoInl op a b = do
  inline check op a b   -- constant folding
  noinline check op a b -- lowering of PrimOp
{-# INLINE checkSubInlNoInl #-}

main :: IO ()
main = do
  -- Overflow.
  checkSubInlNoInl subWordC# 1## 3##
  checkSubInlNoInl addWordC# (unW# (inline maxBound)) 3##

  -- No overflow.
  checkSubInlNoInl subWordC# 5## 2##
  checkSubInlNoInl addWordC# (unW# (inline maxBound-1)) 1##
