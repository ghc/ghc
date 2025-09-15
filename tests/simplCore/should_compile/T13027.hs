{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module T13027 (insert) where

import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)

data Set a = Bin {-# UNPACK #-} !Size !a !(Set a) !(Set a)
           | Tip

type Size = Int

insert :: Ord a => a -> Set a -> Set a
insert = go
  where
    go :: Ord a => a -> Set a -> Set a
    go !x Tip = Bin 1 x Tip Tip
    go !x t@(Bin sz y l r) = case compare x y of
        LT | l' `ptrEq` l -> t
           | otherwise -> undefined -- balanceL y l' r
           where !l' = go x l
        GT | r' `ptrEq` r -> t
           | otherwise -> undefined -- balanceR y l r'
           where !r' = go x r
        EQ | x `ptrEq` y -> t
           | otherwise -> Bin sz x l r
{-# INLINABLE insert #-}

ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y)
{-# INLINE ptrEq #-}
