module Base (
              Map(..)
            , lookup
            , empty
            , insert
            ) where

import Prelude hiding (lookup)
import Good

empty :: Map k a
empty = Tip

data Map k a  = Bin !k a !(Map k a) | Tip

insert :: Ord k => k -> a -> Map k a -> Map k a
insert = go
  where
    go :: Ord k => k -> a -> Map k a -> Map k a
    go kx x Tip = singleton kx x
    go kx x (Bin ky y r) =
        case compare kx ky of
            EQ -> Bin kx x r
            LT -> Bin ky y (go kx x r)
            GT -> Bin ky y (go kx x r)
{-# INLINABLE insert #-}

singleton :: k -> a -> Map k a
singleton k x = Bin k x Tip

lookup :: Eq k => k -> Map k a -> Maybe a
lookup = go
  where
    go x _ | x `seq` False = undefined
    go _ Tip = Nothing
    go k (Bin kx x r) = case k == kx of
      False -> go k r
      True -> Just x
{-# INLINABLE lookup #-}

instance Good k => Good (Map k a) where
  isGood Tip = True
  isGood (Bin k _ r) = isGood k && isGood r
