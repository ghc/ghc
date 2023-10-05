{-# LANGUAGE MagicHash, BangPatterns #-}

module T13331 (naiveInsertInt) where

data Map k a = Bin !Int !k a (Map k a) (Map k a)
             | Tip

singleton :: k -> a -> Map k a
singleton k a = Bin 1 k a Tip Tip

balanceL :: k -> a -> Map k a -> Map k a -> Map k a
balanceL !_ _ !_ !_ = undefined
{-# NOINLINE balanceL #-}

balanceR :: k -> a -> Map k a -> Map k a -> Map k a
balanceR !_ _ !_ !_ = undefined
{-# NOINLINE balanceR #-}

-- | Should not unbox `kx`.
naiveInsertInt :: Int -> a -> Map Int a -> Map Int a
naiveInsertInt !kx x Tip = singleton kx x
naiveInsertInt !kx x t@(Bin sz ky y l r) =
    case compare kx ky of
        LT -> balanceL ky y l' r
           where !l' = naiveInsertInt kx x l
        GT -> balanceR ky y l r'
           where !r' = naiveInsertInt kx x r
        EQ -> Bin sz kx x l r

