-- This is an internal module with a naive set implementation,
-- solely for the purposes of `Data.List.{,NonEmpty.}nubOrd{,By}`.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Data.List.NubOrdSet (
  NubOrdSet,
  empty,
  member,
  insert,
) where

import GHC.Internal.Data.Bool (Bool(..))
import GHC.Internal.Data.Function ((.))
import Data.Ord (Ordering(..))

-- | Implemented as a red-black tree, a la Okasaki.
data NubOrdSet a
  = Empty
  | NodeRed !(NubOrdSet a) !a !(NubOrdSet a)
  | NodeBlack !(NubOrdSet a) !a !(NubOrdSet a)

empty :: NubOrdSet a
empty = Empty

member :: (a -> a -> Ordering) -> a -> NubOrdSet a -> Bool
member cmp = member'
  where
    member' !x = go
      where
        go = \case
          Empty -> False
          NodeRed left center right -> chooseWay left center right
          NodeBlack left center right -> chooseWay left center right

        chooseWay left center right = case cmp x center of
            LT -> go left
            EQ -> True
            GT -> go right
{-# INLINE member #-}

insert :: (a -> a -> Ordering) -> a -> NubOrdSet a -> NubOrdSet a
insert cmp = insert'
  where
    insert' !x = blacken . go
      where
        go node = case node of
          Empty -> NodeRed Empty x Empty
          NodeRed left center right -> case cmp x center of
            LT -> NodeRed (go left) center right
            EQ -> node
            GT -> NodeRed left center (go right)
          NodeBlack left center right -> case cmp x center of
            LT -> balanceBlackLeft (go left) center right
            EQ -> node
            GT -> balanceBlackRight left center (go right)

    blacken node = case node of
      Empty -> Empty
      NodeRed left center right -> NodeBlack left center right
      NodeBlack{} -> node
{-# INLINE insert #-}

balanceBlackLeft :: NubOrdSet a -> a -> NubOrdSet a -> NubOrdSet a
balanceBlackLeft (NodeRed (NodeRed a b c) d e) f g =
  NodeRed (NodeBlack a b c) d (NodeBlack e f g)
balanceBlackLeft (NodeRed a b (NodeRed c d e)) f g =
  NodeRed (NodeBlack a b c) d (NodeBlack e f g)
balanceBlackLeft left center right =
  NodeBlack left center right
{-# INLINE balanceBlackLeft #-}

balanceBlackRight :: NubOrdSet a -> a -> NubOrdSet a -> NubOrdSet a
balanceBlackRight a b (NodeRed (NodeRed c d e) f g) =
  NodeRed (NodeBlack a b c) d (NodeBlack e f g)
balanceBlackRight a b (NodeRed c d (NodeRed e f g)) =
  NodeRed (NodeBlack a b c) d (NodeBlack e f g)
balanceBlackRight left center right =
  NodeBlack left center right
{-# INLINE balanceBlackRight #-}
