{-# LANGUAGE BangPatterns #-}

-- This is a non-exposed internal module
--
-- The code in this module has been ripped from containers-0.5.5.1:Data.Map.Base [1] almost
-- verbatimely to avoid a dependency of 'template-haskell' on the containers package.
--
-- [1] see https://hackage.haskell.org/package/containers-0.5.5.1
--
-- The original code is BSD-licensed and copyrighted by Daan Leijen, Andriy Palamarchuk, et al.

module Language.Haskell.TH.Lib.Map
    ( Map
    , empty
    , insert
    , Language.Haskell.TH.Lib.Map.lookup
    ) where

data Map k a  = Bin {-# UNPACK #-} !Size !k a !(Map k a) !(Map k a)
              | Tip

type Size     = Int

empty :: Map k a
empty = Tip
{-# INLINE empty #-}

singleton :: k -> a -> Map k a
singleton k x = Bin 1 k x Tip Tip
{-# INLINE singleton #-}

size :: Map k a -> Int
size Tip              = 0
size (Bin sz _ _ _ _) = sz
{-# INLINE size #-}

lookup :: Ord k => k -> Map k a -> Maybe a
lookup = go
  where
    go _ Tip = Nothing
    go !k (Bin _ kx x l r) = case compare k kx of
      LT -> go k l
      GT -> go k r
      EQ -> Just x
{-# INLINABLE lookup #-}


insert :: Ord k => k -> a -> Map k a -> Map k a
insert = go
  where
    go :: Ord k => k -> a -> Map k a -> Map k a
    go !kx x Tip = singleton kx x
    go !kx x (Bin sz ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go kx x l) r
            GT -> balanceR ky y l (go kx x r)
            EQ -> Bin sz kx x l r
{-# INLINABLE insert #-}

balanceL :: k -> a -> Map k a -> Map k a -> Map k a
balanceL k x l r = case r of
  Tip -> case l of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x l Tip
           (Bin _ lk lx Tip (Bin _ lrk lrx _ _)) -> Bin 3 lrk lrx (Bin 1 lk lx Tip Tip) (Bin 1 k x Tip Tip)
           (Bin _ lk lx ll@(Bin _ _ _ _ _) Tip) -> Bin 3 lk lx ll (Bin 1 k x Tip Tip)
           (Bin ls lk lx ll@(Bin lls _ _ _ _) lr@(Bin lrs lrk lrx lrl lrr))
             | lrs < ratio*lls -> Bin (1+ls) lk lx ll (Bin (1+lrs) k x lr Tip)
             | otherwise -> Bin (1+ls) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+size lrr) k x lrr Tip)

  (Bin rs _ _ _ _) -> case l of
           Tip -> Bin (1+rs) k x Tip r

           (Bin ls lk lx ll lr)
              | ls > delta*rs  -> case (ll, lr) of
                   (Bin lls _ _ _ _, Bin lrs lrk lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lk lx ll (Bin (1+rs+lrs) k x lr r)
                     | otherwise -> Bin (1+ls+rs) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+rs+size lrr) k x lrr r)
                   (_, _) -> error "Failure in Data.Map.balanceL"
              | otherwise -> Bin (1+ls+rs) k x l r
{-# NOINLINE balanceL #-}

balanceR :: k -> a -> Map k a -> Map k a -> Map k a
balanceR k x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x Tip r
           (Bin _ rk rx Tip rr@(Bin _ _ _ _ _)) -> Bin 3 rk rx (Bin 1 k x Tip Tip) rr
           (Bin _ rk rx (Bin _ rlk rlx _ _) Tip) -> Bin 3 rlk rlx (Bin 1 k x Tip Tip) (Bin 1 rk rx Tip Tip)
           (Bin rs rk rx rl@(Bin rls rlk rlx rll rlr) rr@(Bin rrs _ _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rk rx (Bin (1+rls) k x Tip rl) rr
             | otherwise -> Bin (1+rs) rlk rlx (Bin (1+size rll) k x Tip rll) (Bin (1+rrs+size rlr) rk rx rlr rr)

  (Bin ls _ _ _ _) -> case r of
           Tip -> Bin (1+ls) k x l Tip

           (Bin rs rk rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (Bin rls rlk rlx rll rlr, Bin rrs _ _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rk rx (Bin (1+ls+rls) k x l rl) rr
                     | otherwise -> Bin (1+ls+rs) rlk rlx (Bin (1+ls+size rll) k x l rll) (Bin (1+rrs+size rlr) rk rx rlr rr)
                   (_, _) -> error "Failure in Data.Map.balanceR"
              | otherwise -> Bin (1+ls+rs) k x l r
{-# NOINLINE balanceR #-}

delta,ratio :: Int
delta = 3
ratio = 2
