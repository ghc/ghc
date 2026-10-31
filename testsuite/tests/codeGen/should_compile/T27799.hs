-- Test for #27799: the latch of the innermost loop (the block with two
-- movsd instructions) should end in a conditional jump back to the loop's
-- body ("cmpq; jl body"), not in a conditional jump out followed by an
-- unconditional jump back ("cmpq; jge exit; jmp body"). Which shape the
-- layout gave followed the order of uniques, so -fobject-determinism and
-- -fno-object-determinism gave different ones; -O2 -fobject-determinism
-- gave the split latch.
{-# LANGUAGE BangPatterns #-}
module T27799 (fill) where

import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import System.IO.Unsafe (unsafeDupablePerformIO)

idx :: Ptr Int -> Int -> Int
{-# INLINE idx #-}
idx p i = unsafeDupablePerformIO (peekElemOff p i)

fill :: Ptr Double -> Ptr Double -> Ptr Int -> Int -> Int -> IO Int
{-# NOINLINE fill #-}
fill out v p !sInner !tInner = go 0 0 0
  where
    go !lev !outPos !baseOff
      | lev >= 2 =
          let !oEnd = outPos + sInner
              inner !o !src
                | o >= oEnd = return ()
                | otherwise = do
                    x <- peekElemOff v src
                    pokeElemOff out o x
                    inner (o + 1) (src + tInner)
          in  inner outPos baseOff >> return (outPos + sInner)
      | otherwise =
          let !n  = idx p lev
              !st = idx p (lev + 4)
              dim !k !op !boff
                | k <= 0    = return op
                | otherwise = go (lev + 1) op boff >>= \op' -> dim (k - 1) op' (boff + st)
          in  dim n outPos baseOff
