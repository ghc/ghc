{-# LANGUAGE CPP #-}
module LookupGE_IntMap where

import Prelude hiding (null)
import Data.IntMap.Internal

lookupGE1 :: Key -> IntMap a -> Maybe (Key,a)
lookupGE1 k m =
    case splitLookup k m of
        (_,Just v,_)  -> Just (k,v)
        (_,Nothing,r) -> findMinMaybe r

lookupGE2 :: Key -> IntMap a -> Maybe (Key,a)
lookupGE2 k t = case t of
    Bin _ m l r | m < 0 -> if k >= 0
      then go l
      else case go r of
        Nothing -> Just $ findMin l
        justx -> justx
    _ -> go t
  where
    go (Bin p m l r)
      | nomatch k p m = if k < p
        then Just $ findMin l
        else Nothing
      | zero k m = case go l of
        Nothing -> Just $ findMin r
        justx -> justx
      | otherwise = go r
    go (Tip ky y)
      | k > ky = Nothing
      | otherwise = Just (ky, y)
    go Nil = Nothing

lookupGE3 :: Key -> IntMap a -> Maybe (Key,a)
lookupGE3 k t = k `seq` case t of
    Bin _ m l r | m < 0 -> if k >= 0
      then go Nothing l
      else go (Just (findMin l)) r
    _ -> go Nothing t
  where
    go def (Bin p m l r)
      | nomatch k p m = if k < p then Just $ findMin l else def
      | zero k m  = go (Just $ findMin r) l
      | otherwise = go def r
    go def (Tip ky y)
      | k > ky    = def
      | otherwise = Just (ky, y)
    go def Nil  = def

lookupGE4 :: Key -> IntMap a -> Maybe (Key,a)
lookupGE4 k t = k `seq` case t of
    Bin _ m l r | m < 0 -> if k >= 0 then go Nil l
                                     else go l r
    _ -> go Nil t
  where
    go def (Bin p m l r)
      | nomatch k p m = if k < p then fMin l else fMin def
      | zero k m  = go r l
      | otherwise = go def r
    go def (Tip ky y)
      | k > ky    = fMin def
      | otherwise = Just (ky, y)
    go def Nil  = fMin def

    fMin :: IntMap a -> Maybe (Key, a)
    fMin Nil = Nothing
    fMin (Tip ky y) = Just (ky, y)
    fMin (Bin _ _ l _) = fMin l

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | /O(log n)/. The minimal key of the map.
findMinMaybe :: IntMap a -> Maybe (Key, a)
findMinMaybe m
  | null m = Nothing
  | otherwise = Just (findMin m)

#ifdef TESTING
-------------------------------------------------------------------------------
-- Properties:
-------------------------------------------------------------------------------

prop_lookupGE12 :: Int -> [Int] -> Bool
prop_lookupGE12 x xs = case fromList $ zip xs xs of m -> lookupGE1 x m == lookupGE2 x m

prop_lookupGE13 :: Int -> [Int] -> Bool
prop_lookupGE13 x xs = case fromList $ zip xs xs of m -> lookupGE1 x m == lookupGE3 x m

prop_lookupGE14 :: Int -> [Int] -> Bool
prop_lookupGE14 x xs = case fromList $ zip xs xs of m -> lookupGE1 x m == lookupGE4 x m
#endif
