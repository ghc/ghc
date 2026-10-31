{-# LANGUAGE BangPatterns #-}
-- The constant folder (GHC.Cmm.Opt) must not turn (i - s) + (-1), which this
-- copy of containers' Set.elemAt computes for its right step at -O2, into
-- i + ((-1) - s), which costs x86 one instruction more.
module Elem (elemAt, Set(..)) where

data Set a = Bin {-# UNPACK #-} !Int !a !(Set a) !(Set a) | Tip

size :: Set a -> Int
size Tip = 0
size (Bin sz _ _ _) = sz

elemAt :: Int -> Set a -> a
elemAt !_ Tip = error "elemAt: index out of range"
elemAt i (Bin _ x l r)
  = case compare i sizeL of
      LT -> elemAt i l
      GT -> elemAt (i-sizeL-1) r
      EQ -> x
  where
    sizeL = size l
