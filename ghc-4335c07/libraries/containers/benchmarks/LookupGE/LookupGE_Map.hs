{-# LANGUAGE BangPatterns, CPP #-}
module LookupGE_Map where

import Data.Map.Internal

lookupGE1 :: Ord k => k -> Map k a -> Maybe (k,a)
lookupGE1 k m =
    case splitLookup k m of
        (_,Just v,_)  -> Just (k,v)
        (_,Nothing,r) -> findMinMaybe r
{-# INLINABLE lookupGE1 #-}

lookupGE2 :: Ord k => k -> Map k a -> Maybe (k,a)
lookupGE2 = go
  where
    go !_ Tip = Nothing
    go !k (Bin _ kx x l r) =
        case compare k kx of
            LT -> case go k l of
                    Nothing -> Just (kx,x)
                    ret -> ret
            GT -> go k r
            EQ -> Just (kx,x)
{-# INLINABLE lookupGE2 #-}

lookupGE3 :: Ord k => k -> Map k a -> Maybe (k,a)
lookupGE3 = go Nothing
  where
    go def !_ Tip = def
    go def !k (Bin _ kx x l r) =
        case compare k kx of
            LT -> go (Just (kx,x)) k l
            GT -> go def k r
            EQ -> Just (kx,x)
{-# INLINABLE lookupGE3 #-}

lookupGE4 :: Ord k => k -> Map k a -> Maybe (k,a)
lookupGE4 k = k `seq` goNothing
  where
    goNothing Tip = Nothing
    goNothing (Bin _ kx x l r) = case compare k kx of
                                   LT -> goJust kx x l
                                   EQ -> Just (kx, x)
                                   GT -> goNothing r

    goJust ky y Tip = Just (ky, y)
    goJust ky y (Bin _ kx x l r) = case compare k kx of
                                     LT -> goJust kx x l
                                     EQ -> Just (kx, x)
                                     GT -> goJust ky y r
{-# INLINABLE lookupGE4 #-}

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

findMinMaybe :: Map k a -> Maybe (k,a)
findMinMaybe (Bin _ kx x Tip _)  = Just (kx,x)
findMinMaybe (Bin _ _  _ l _)    = findMinMaybe l
findMinMaybe Tip                 = Nothing

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
