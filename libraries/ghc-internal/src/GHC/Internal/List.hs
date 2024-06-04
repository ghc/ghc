{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE BangPatterns, MagicHash #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.List
-- Copyright   :  (c) The University of Glasgow 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The List data type and its operations
--
-----------------------------------------------------------------------------

module GHC.Internal.List (

   -- * The list data type
   List,

   -- * List-monomorphic Foldable methods and misc functions
   foldr, foldr', foldr1,
   foldl, foldl', foldl1,
   null, length, elem, notElem,
   maximum, minimum, sum, product, and, or, any, all,

   -- * Other functions
   foldl1', concat, concatMap,
   map, (++), filter, lookup,
   head, last, tail, init, uncons, unsnoc, (!?), (!!),
   scanl, scanl1, scanl', scanr, scanr1,
   iterate, iterate', repeat, replicate, cycle,
   take, drop, splitAt, takeWhile, dropWhile, span, break, reverse,
   zip, zip3, zipWith, zipWith3, unzip, unzip3,
   errorEmptyList,

   -- * GHC List fusion
   augment, build,

   -- * Enumeration
   eftInt,

 ) where

import GHC.Internal.Data.Maybe
import GHC.Internal.Base
import GHC.Internal.Num (Num(..))
import GHC.Num.Integer (Integer)
import GHC.Internal.Stack.Types (HasCallStack)

infixl 9  !?, !!
infix  4 `elem`, `notElem`

data Tuple a b = !a :!: !b
data Option a = None | Some !a

eftIntS :: Int# -> Int# -> Stream Int
eftIntS x y = Stream next (I# x) where
  next !s
    | s <= I# y = Yield s (s + 1)
    | otherwise = Done
  {-# INLINE next #-}
{-# INLINE eftIntS #-}

eftInt :: Int# -> Int# -> [Int]
eftInt = \x y -> cheapUnstream (eftIntS x y)
{-# INLINE eftInt #-}

-- unfoldrS :: (a -> Maybe (a, b)) -> a -> Stream b
-- unfoldrS f x0 = Stream next x0 where
--   next s =
--     case f s of
--       Just (s', x) -> Yield x s'
--       Nothing -> Done
--   {-# INLINE next #-}
-- {-# INLINE unfoldrS #-}

data ZipState s1 a s2
  = ZipState1 !s1   !s2
  | ZipState2 !s1 a !s2

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS (Stream next1 s01) (Stream next2 s02) = Stream next' (ZipState1 s01 s02) where
  next' (ZipState1 s1 s2) =
    case next1 s1 of
      Yield x s1' ->
        case next2 s2 of
          Yield y s2' ->
            Yield (x, y) (ZipState1 s1' s2')
          Skip s2' -> Skip (ZipState2 s1' x s2')
          Done -> Done
      Skip s1' -> Skip (ZipState1 s1' s2)
      Done -> Done
  next' (ZipState2 s1' x s2) =
    case next2 s2 of
      Yield y s2' -> Yield (x, y) (ZipState1 s1' s2')
      Skip s2' -> Skip (ZipState2 s1' x s2')
      Done -> Done
  {-# INLINE next' #-}
{-# INLINE zipS #-}

data Zip3State s1 a s2 b s3
  = Zip3State1 !s1   !s2   !s3
  | Zip3State2 !s1 a !s2   !s3
  | Zip3State3 !s1 a !s2 b !s3

zip3S :: Stream a -> Stream b -> Stream c -> Stream (a, b, c)
zip3S (Stream next1 s01) (Stream next2 s02) (Stream next3 s03) = Stream next' (Zip3State1 s01 s02 s03) where
  next' (Zip3State1 s1 s2 s3) =
    case next1 s1 of
      Yield x s1' ->
        case next2 s2 of
          Yield y s2' ->
            case next3 s3 of
              Yield z s3' -> Yield (x, y, z) (Zip3State1 s1' s2' s3')
              Skip s3' -> Skip (Zip3State3 s1' x s2' y s3')
              Done -> Done
          Skip s2' -> Skip (Zip3State2 s1' x s2' s3)
          Done -> Done
      Skip s1' -> Skip (Zip3State1 s1' s2 s3)
      Done -> Done
  next' (Zip3State2 s1' x s2 s3) =
    case next2 s2 of
      Yield y s2' ->
        case next3 s3 of
          Yield z s3' -> Yield (x,y,z) (Zip3State1 s1' s2' s3')
          Skip s3' -> Skip (Zip3State3 s1' x s2' y s3')
          Done -> Done
      Skip s2' -> Skip (Zip3State2 s1' x s2' s3)
      Done -> Done
  next' (Zip3State3 s1' x s2' y s3) =
    case next3 s3 of
      Yield z s3' -> Yield (x,y,z) (Zip3State1 s1' s2' s3')
      Skip s3' -> Skip (Zip3State3 s1' x s2' y s3')
      Done -> Done
  {-# INLINE next' #-}
{-# INLINE zip3S #-}

zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f (Stream next1 s01) (Stream next2 s02) = Stream next' (ZipState1 s01 s02) where
  next' (ZipState1 s1 s2) =
    case next1 s1 of
      Yield x s1' ->
        case next2 s2 of
          Yield y s2' ->
            Yield (f x y) (ZipState1 s1' s2')
          Skip s2' -> Skip (ZipState2 s1' x s2')
          Done -> Done
      Skip s1' -> Skip (ZipState1 s1' s2)
      Done -> Done
  next' (ZipState2 s1' x s2) =
    case next2 s2 of
      Yield y s2' -> Yield (f x y) (ZipState1 s1' s2')
      Skip s2' -> Skip (ZipState2 s1' x s2')
      Done -> Done
  {-# INLINE next' #-}
{-# INLINE zipWithS #-}

zipWith3S :: (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
zipWith3S f (Stream next1 s01) (Stream next2 s02) (Stream next3 s03) = Stream next' (Zip3State1 s01 s02 s03) where
  next' (Zip3State1 s1 s2 s3) =
    case next1 s1 of
      Yield x s1' ->
        case next2 s2 of
          Yield y s2' ->
            case next3 s3 of
              Yield z s3' -> Yield (f x y z) (Zip3State1 s1' s2' s3')
              Skip s3' -> Skip (Zip3State3 s1' x s2' y s3')
              Done -> Done
          Skip s2' -> Skip (Zip3State2 s1' x s2' s3)
          Done -> Done
      Skip s1' -> Skip (Zip3State1 s1' s2 s3)
      Done -> Done
  next' (Zip3State2 s1' x s2 s3) =
    case next2 s2 of
      Yield y s2' ->
        case next3 s3 of
          Yield z s3' -> Yield (f x y z) (Zip3State1 s1' s2' s3')
          Skip s3' -> Skip (Zip3State3 s1' x s2' y s3')
          Done -> Done
      Skip s2' -> Skip (Zip3State2 s1' x s2' s3)
      Done -> Done
  next' (Zip3State3 s1' x s2' y s3) =
    case next3 s3 of
      Yield z s3' -> Yield (f x y z) (Zip3State1 s1' s2' s3')
      Skip s3' -> Skip (Zip3State3 s1' x s2' y s3')
      Done -> Done
  {-# INLINE next' #-}
{-# INLINE zipWith3S #-}

-- unzipS :: Stream (a, b) -> (Stream a, Stream b)
-- unzipS s = (mapS fst s, mapS snd s)
-- {-# INLINE unzipS #-}

unzipS :: Stream (a, b) -> ([a], [b])
unzipS (Stream next s0) = go s0 where
  go s =
    case next s of
      Yield (x, y) s' -> let (xs, ys) = go s' in (x:xs, y:ys)
      Skip s' -> go s'
      Done -> ([], [])
{-# INLINE unzipS #-}

-- unzip3S :: Stream (a, b, c) -> (Stream a, Stream b, Stream c)
-- unzip3S s = (mapS (\(x,_,_) -> x) s, mapS (\(_,x,_) -> x) s, mapS (\(_,_,x) -> x) s)
-- {-# INLINE unzip3S #-}

unzip3S :: Stream (a, b, c) -> ([a], [b], [c])
unzip3S (Stream next s0) = go s0 where
  go !s =
    case next s of
      Yield (x, y, z) s' -> let (xs, ys, zs) = go s' in (x:xs, y:ys, z:zs)
      Skip s' -> go s'
      Done -> ([], [], [])
{-# INLINE unzip3S #-}

foldrS' :: (a -> b -> b) -> b -> Stream a -> b
foldrS' k z (Stream next s0) = go s0 where
  go !s =
    case next s of
      Yield x s' -> k x $! go s'
      Skip s' -> go s'
      Done -> z
{-# INLINE foldrS' #-}

foldr1S :: (a -> a -> a) -> Stream a -> a
foldr1S f (Stream next s0) = go1 s0 where
  go1 !s =
    case next s of
      Yield x s' -> go2 x s'
      Skip s' -> go1 s'
      Done -> errorEmptyList "foldr1"
  go2 x !s =
    case next s of
      Yield y s' -> f x (go2 y s')
      Skip s' -> go2 x s'
      Done -> x
{-# INLINE foldr1S #-}

nullS :: Stream a -> Bool
nullS (Stream next s0) = go s0 where
  go !s =
    case next s of
      Yield{} -> False
      Skip s' -> go s'
      Done -> True
{-# INLINE nullS #-}

lengthS :: Stream a -> Int
lengthS (Stream next s0) = go 0 s0 where
  go !n !s =
    case next s of
      Yield _ s' -> go (n + 1) s'
      Skip s' -> go n s'
      Done -> n
{-# INLINE lengthS #-}

elemS :: Eq a => a -> Stream a -> Bool
elemS x0 (Stream next s0) = go s0 where
  go s =
    case next s of
      Yield x s' -> x == x0 || go s'
      Skip s' -> go s'
      Done -> False
{-# INLINE elemS #-}

notElemS :: Eq a => a -> Stream a -> Bool
notElemS x0 (Stream next s0) = go s0 where
  go s =
    case next s of
      Yield x s' -> x /= x0 && go s'
      Skip s' -> go s'
      Done -> True
{-# INLINE notElemS #-}

maximumS :: Ord a => Stream a -> a
maximumS (Stream next s0) = go1 s0 where
  go1 !s =
    case next s of
      Yield x s' -> go2 x s'
      Skip s' -> go1 s'
      Done -> errorEmptyList "maximum"
  go2 x !s =
    case next s of
      Yield y s'
        | y > x -> go2 y s'
        | otherwise -> go2 x s'
      Skip s' -> go2 x s'
      Done -> x
{-# INLINE maximumS #-}

minimumS :: Ord a => Stream a -> a
minimumS (Stream next s0) = go1 s0 where
  go1 !s =
    case next s of
      Yield x s' -> go2 x s'
      Skip s' -> go1 s'
      Done -> errorEmptyList "minimum"
  go2 x !s =
    case next s of
      Yield y s'
        | y < x -> go2 y s'
        | otherwise -> go2 x s'
      Skip s' -> go2 x s'
      Done -> x
{-# INLINE minimumS #-}

takeS :: Int -> Stream a -> Stream a
takeS n (Stream next s0) = Stream next' (s0 :!: n) where
  next' (_ :!: 0) = Done
  next' (s :!: i) =
    case next s of
      Yield x s' -> Yield x (s' :!: (i - 1))
      Skip s' -> Skip (s' :!: i)
      Done -> Done
  {-# INLINE next' #-}
{-# INLINE takeS #-}

-- dropS :: Int -> Stream a -> Stream a
-- dropS n (Stream next s0) = Stream next (f n s0) where
--   f i s
--     | i <= 0 = s
--     | otherwise =
--       case next s of
--         Yield _ s' -> f (i - 1) s'
--         Skip s' -> f i s'
--         Done -> s
-- {-# INLINE dropS #-}

dropS :: Int -> Stream a -> Stream a
dropS n (Stream next s0) = Stream next' (n :!: s0) where
  next' (i :!: s)
    | i > 0 =
      case next s of
        Yield _ s' -> Skip ((i - 1) :!: s')
        Skip s' -> Skip (i :!: s')
        Done -> Done
    | otherwise =
      case next s of
        Yield x s' -> Yield x (i :!: s')
        Skip s' -> Skip (i :!: s')
        Done -> Done
  {-# INLINE next' #-}
{-# INLINE dropS #-}

cheapSplitAtS :: Int -> Stream a -> ([a], [a])
cheapSplitAtS n s = (cheapUnstream (takeS n s), cheapUnstream (dropS n s))
{-# INLINE cheapSplitAtS #-}

splitAtS :: Int -> Stream a -> ([a], [a])
splitAtS n (Stream next s0) = go1 n s0 where
  go1 !i !s
    | i > 0 =
      case next s of
        Yield x s' -> let (xs,ys) = go1 (i - 1) s' in (x : xs, ys)
        Skip s'    -> go1 i s'
        Done       -> ([], [])
    | otherwise = ([], go2 s)
  go2 !s =
    case next s of
      Yield x s' -> x : go2 s'
      Skip s' -> go2 s'
      Done -> []
{-# INLINE splitAtS #-}

takeWhileS :: (a -> Bool) -> Stream a -> Stream a
takeWhileS p (Stream next s0) = Stream next' s0 where
  next' !s =
    case next s of
      Yield x s'
        | p x -> Yield x s'
        | otherwise -> Done
      Skip s' -> Skip s'
      Done -> Done
{-# INLINE takeWhileS #-}

dropWhileS :: (a -> Bool) -> Stream a -> Stream a
dropWhileS p (Stream next s0) = Stream next' (True :!: s0) where
  next' (True :!: s) =
      case next s of
        Yield x s'
          | p x -> Skip (True :!: s')
          | otherwise -> Yield x (False :!: s')
        Skip s' -> Skip (True :!: s')
        Done -> Done
  next' (False :!: s) =
      case next s of
        Yield x s' -> Yield x (False :!: s')
        Skip s' -> Skip (False :!: s')
        Done -> Done
  {-# INLINE next' #-}
{-# INLINE dropWhileS #-}

-- nubS :: Eq a => Stream a -> Stream a
-- nubS (Stream next s0) = Stream next' ([] :!: s0) where
--   next' (xs :!: s) =
--     case next s of
--       Yield x s'
--         | x `elem` xs -> Skip (xs :!: s')
--         | otherwise -> Yield x ((x : xs) :!: s')
--       Skip s' -> Skip (xs :!: s')
--       Done -> Done
--   {-# INLINE next' #-}
-- {-# INLINE nubS #-}

-- spanS :: (a -> Bool) -> Stream a -> (Stream a, Stream a)
-- spanS f s = (takeWhileS f s, dropWhileS f s)
-- {-# INLINE spanS #-}

spanS :: (a -> Bool) -> Stream a -> ([a], [a])
spanS p (Stream next s0) = go1 s0 where
  go1 !s =
    case next s of
      Yield x s'
        | p x -> let (xs, ys) = go1 s' in (x : xs, ys)
        | otherwise -> ([], x : go2 s')
      Skip s' -> go1 s'
      Done -> ([],[])
  go2 !s =
    case next s of
      Yield x s' -> x : go2 s'
      Skip s' -> go2 s'
      Done -> []
{-# INLINE spanS #-}

breakS :: (a -> Bool) -> Stream a -> ([a], [a])
breakS f = spanS (not . f)
{-# INLINE breakS #-}

reverseS :: Stream a -> [a]
reverseS = foldlS' (flip (:)) []
{-# INLINE reverseS #-}

foldlS :: (b -> a -> b) -> b -> Stream a -> b
foldlS k z (Stream next s0) = go z s0 where
  go acc !s =
    case next s of
      Yield x s' -> go (k acc x) s'
      Skip s' -> go acc s'
      Done -> acc
{-# INLINE foldlS #-}

foldlS' :: (b -> a -> b) -> b -> Stream a -> b
foldlS' k z (Stream next s0) = go z s0 where
  go !acc !s =
    case next s of
      Yield x s' -> go (k acc x) s'
      Skip s' -> go acc s'
      Done -> acc
{-# INLINE foldlS' #-}

foldl1S :: (a -> a -> a) -> Stream a -> a
foldl1S f (Stream next s0) = go1 s0 where
  go1 !s =
    case next s of
      Yield x s' -> go2 x s'
      Skip s' -> go1 s'
      Done -> errorEmptyList "foldl1"
  go2 acc !s =
    case next s of
      Yield x s' -> go2 (f acc x) s'
      Skip s' -> go2 acc s'
      Done -> acc
{-# INLINE foldl1S #-}

-- consumer
sumS :: Num a => Stream a -> a
sumS (Stream next s0) = go 0 s0 where
  go !acc !s = case next s of
    Done -> acc
    Skip s' -> go acc s'
    Yield x s' -> go (acc + x) s'
{-# INLINE sumS #-}

productS :: Num a => Stream a -> a
productS (Stream next s0) = go 1 s0 where
  go !acc !s = case next s of
    Done -> acc
    Skip s' -> go acc s'
    Yield x s' -> go (acc * x) s'
{-# INLINE productS #-}

andS :: Stream Bool -> Bool
andS (Stream next s0) = go True s0 where
  go !acc !s = case next s of
    Done -> acc
    Skip s' -> go acc s'
    Yield x s' -> go (acc && x) s'
{-# INLINE andS #-}

orS :: Stream Bool -> Bool
orS (Stream next s0) = go False s0 where
  go !acc !s = case next s of
    Done -> acc
    Skip s' -> go acc s'
    Yield x s' -> go (acc || x) s'
{-# INLINE orS #-}

anyS :: (a -> Bool) -> Stream a -> Bool
anyS p (Stream next s0) = go False s0 where
  go !acc !s = case next s of
    Done -> acc
    Skip s' -> go acc s'
    Yield x s' -> go (acc || p x) s'
{-# INLINE anyS #-}

allS :: (a -> Bool) -> Stream a -> Bool
allS f (Stream next s0) = go True s0 where
  go !acc !s = case next s of
    Done -> acc
    Skip s' -> go acc s'
    Yield x s' -> go (acc && f x) s'
{-# INLINE allS #-}

foldl1S' :: (a -> a -> a) -> Stream a -> a
foldl1S' f (Stream next s0) = go1 s0 where
  go1 !s =
    case next s of
      Yield x s' -> go2 x s'
      Skip s' -> go1 s'
      Done -> errorEmptyList "foldl1"
  go2 !acc !s =
    case next s of
      Yield x s' -> go2 (f acc x) s'
      Skip s' -> go2 acc s'
      Done -> acc
{-# INLINE foldl1S' #-}

filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (Stream next s0) = Stream next' s0 where
  next' !s =
    case next s of
      Yield x s'
        | p x -> Yield x s'
        | otherwise -> Skip s'
      Skip s' -> Skip s'
      Done -> Done
  {-# INLINE next' #-}
{-# INLINE filterS #-}

lookupS :: Eq a => a -> Stream (a, b) -> Maybe b
lookupS x0 (Stream next s0) = go s0 where
  go !s =
    case next s of
      Yield (x,y) s'
        | x0 == x -> Just y
        | otherwise -> go s'
      Skip s' -> go s'
      Done -> Nothing
{-# INLINE lookupS #-}

-- findS :: (a -> Bool) -> Stream a -> Maybe a
-- findS p (Stream next s0) = go s0 where
--   go !s =
--     case next s of
--       Yield x s'
--         | p x -> Just x
--         | otherwise -> go s'
--       Skip s' -> go s'
--       Done -> Nothing
-- {-# INLINE findS #-}
--
-- findIndexS :: (a -> Bool) -> Stream a -> Maybe Int
-- findIndexS p (Stream next s0) = go 0 s0 where
--   go !i !s =
--     case next s of
--       Yield x s'
--         | p x -> Just i
--         | otherwise -> go (i + 1) s'
--       Skip s' -> go i s'
--       Done -> Nothing
-- {-# INLINE findIndexS #-}

headS :: Stream a -> a
headS (Stream next s0) = go s0 where
  go !s =
    case next s of
      Yield x _ -> x
      Skip s' -> go s'
      Done -> errorEmptyList "head"
{-# INLINE headS #-}

lastS :: Stream a -> a
lastS (Stream next s0) = go1 s0 where
  go1 !s =
    case next s of
      Yield x s' -> go2 x s'
      Skip s' -> go1 s'
      Done -> errorEmptyList "last"
  go2 x !s =
    case next s of
      Yield x' s' -> go2 x' s'
      Skip s' -> go2 x s'
      Done -> x
{-# INLINE lastS #-}

-- tailS :: Stream a -> Stream a
-- tailS (Stream next s0) = Stream next (tailF s0) where
--   tailF !s =
--     case next s of
--       Yield _ s' -> s'
--       Skip s' -> tailF s'
--       Done -> errorEmptyList "tail"
--
-- initS1 :: Stream a -> Stream a
-- initS1 (Stream next s0) = Stream next' (f s0) where
--   f !s =
--     case next s of
--       Yield x s' -> x :!: s'
--       Skip s' -> f s'
--       Done -> errorEmptyList "init"
--   next' (x :!: s) =
--     case next s of
--       Yield y s' -> Yield x (y :!: s')
--       Skip s' -> Skip (x :!: s')
--       Done -> Done
-- {-# INLINE initS1 #-}

initS :: Stream a -> Stream a
initS (Stream next s0) = Stream next' (Nothing :!: s0) where
  next' (Nothing :!: s) =
    case next s of
      Yield x s' -> Skip (Just x :!: s')
      Skip s' -> Skip (Nothing :!: s')
      Done -> errorEmptyList "init"
  next' (Just x :!: s) =
    case next s of
      Yield y s' -> Yield x (Just y :!: s')
      Skip s' -> Skip (Just x :!: s')
      Done -> Done
{-# INLINE initS #-}

unconsS :: Stream a -> Maybe (a, [a])
unconsS (Stream next s0) = go1 s0 where
  go1 !s =
    case next s of
      Yield x s' -> Just (x, go2 s')
      Skip s' -> go1 s'
      Done -> Nothing
  go2 !s =
    case next s of
      Yield x s' -> x : go2 s'
      Skip s' -> go2 s'
      Done -> []
{-# INLINE unconsS #-}

unsnocS :: Stream a -> Maybe ([a], a)
-- duplicates work:
-- unsnocS s = if nullS s then Nothing else Just (unstream (initS s), lastS s)
unsnocS = foldrS (\x xs -> case xs of Nothing -> Just ([], x); Just (ys,y) -> Just (x:ys,y)) Nothing
{-# INLINE unsnocS #-}

indexHelperS :: Stream a -> Int -> Maybe a
indexHelperS (Stream next s0) i0 = go i0 s0
  where
    go 0 !s =
      case next s of
        Yield x _ -> Just x
        Skip s' -> go 0 s'
        Done -> Nothing
    go !i !s =
      case next s of
        Yield _ s' -> go (i - 1) s'
        Skip s' -> go i s'
        Done -> Nothing
{-# INLINE indexHelperS #-}

unsafeIndexHelperS :: Stream a -> Int -> a
unsafeIndexHelperS (Stream next s0) i0 = go i0 s0
  where
    go !i !s =
      case next s of
        Yield x s' -> if i == 0 then x else go (i - 1) s'
        Skip s' -> go i s'
        Done -> tooLarge i
{-# INLINE unsafeIndexHelperS #-}

scanlS :: (b -> a -> b) -> b -> Stream a -> Stream b
scanlS k z (Stream next s0) = Stream next' (Just z :!: s0) where
  next' (Just x :!: s) =
    case next s of
      Yield y s' -> Yield x (Just (k x y) :!: s')
      Skip s' -> Skip (Just x :!: s')
      Done -> Yield x (Nothing :!: s)
  next' (Nothing :!: _) = Done
  {-# INLINE next' #-}
{-# INLINE scanlS #-}

data Scanl1State a s = Scanl1State1 !s | Scanl1State2 a !s | Scanl1State3

scanl1S :: (a -> a -> a) -> Stream a -> Stream a
scanl1S k (Stream next s0) = Stream next' (Scanl1State1 s0) where
  next' (Scanl1State1 s) =
    case next s of
      Yield x s' -> Skip (Scanl1State2 x s')
      Skip s' -> Skip (Scanl1State1 s')
      Done -> errorEmptyList "scanl1"
  next' (Scanl1State2 x s) =
    case next s of
      Yield y s' -> Yield x (Scanl1State2 (k x y) s')
      Skip s' -> Skip (Scanl1State2 x s')
      Done -> Yield x Scanl1State3
  next' Scanl1State3 = Done
  {-# INLINE next' #-}
{-# INLINE scanl1S #-}

scanlS' :: (b -> a -> b) -> b -> Stream a -> Stream b
scanlS' k z (Stream next s0) = Stream next' (Some (z :!: s0)) where
  next' (Some (x :!: s)) =
    case next s of
      Yield y s' -> Yield x (Some (k x y :!: s'))
      Skip s' -> Skip (Some (x :!: s'))
      Done -> Yield x None
  next' None = Done
  {-# INLINE next' #-}
{-# INLINE scanlS' #-}

iterateS :: (a -> a) -> a -> Stream a
iterateS f = Stream next . L where
  next (L s) = Yield s (L (f s))
  {-# INLINE next #-}
{-# INLINE iterateS #-}

iterateS' :: (a -> a) -> a -> Stream a
iterateS' f = Stream next where
  next s = Yield s (f s)
  {-# INLINE next #-}
{-# INLINE iterateS' #-}

-- repeatS :: a -> Stream a
-- repeatS = Stream next . L where
--   next (L s) = Yield s (L s)
--   {-# INLINE next #-}
-- {-# INLINE repeatS #-}

replicateS :: Int -> a -> Stream a
replicateS n x = Stream next n where
  next s | s <= 0 = Done
         | otherwise = Yield x (s - 1)
  {-# INLINE next #-}
{-# INLINE replicateS #-}

-- cycleS :: Stream a -> Stream a
-- cycleS (Stream next s0) = Stream next' s0 where
--   next' !s =
--     case next s of
--       Yield x s' -> Yield x s'
--       Skip s' -> Skip s'
--       Done -> Skip s0
-- {-# INLINE cycleS #-}

concatS :: Stream [a] -> Stream a
concatS (Stream next s0) = Stream next' (s0 :!: L []) where
  next' (s :!: L []) =
    case next s of
      Yield [] s' -> Skip (s' :!: L [])
      Yield (x:xs) s' -> Yield x (s' :!: L xs)
      Skip s' -> Skip (s' :!: L [])
      Done -> Done
  next' (s :!: L (x:xs)) = Yield x (s :!: L xs)
{-# INLINE concatS #-}

scanrS :: (a -> b -> b) -> b -> Stream a -> [b]
scanrS f q0 = foldrS (\x qs@(q:_) -> f x q : qs) [q0]
{-# INLINE scanrS #-}
scanr1S :: (a -> a -> a) -> Stream a -> [a]
scanr1S f = foldrS (\x qs -> case qs of [] -> [x]; (q:_) -> f x q : qs) []
{-# INLINE scanr1S #-}


-- foldr' :: (a -> b -> b) -> b -> [a] -> b
-- foldr' k z = foldrS' k z . stream
-- {-# INLINE foldr' #-}
-- foldr1 :: (a -> a -> a) -> [a] -> a
-- foldr1 k = foldr1S k . stream
-- {-# INLINE foldr1 #-}
--
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl k z = foldlS k z . stream
-- {-# INLINE foldl #-}
-- foldl' :: (b -> a -> b) -> b -> [a] -> b
-- foldl' k z = foldlS' k z . stream
-- {-# INLINE foldl' #-}
-- foldl1 :: (a -> a -> a) -> [a] -> a
-- foldl1 k = foldl1S k . stream
-- {-# INLINE foldl1 #-}
--
-- null :: [a] -> Bool
-- null = nullS . stream
-- {-# INLINE null #-}
-- length :: [a] -> Int
-- length = lengthS . stream
-- {-# INLINE length #-}
-- elem :: Eq a => a -> [a] -> Bool
-- elem x = elemS x . stream
-- {-# INLINE elem #-}
-- notElem :: Eq a => a -> [a] -> Bool
-- notElem x = notElemS x . stream
-- {-# INLINE notElem #-}
--
-- maximum :: Ord a => [a] -> a
-- maximum = maximumS . stream
-- {-# INLINE maximum #-}
-- minimum :: Ord a => [a] -> a
-- minimum = minimumS . stream
-- {-# INLINE minimum #-}
-- sum :: Num a => [a] -> a
-- sum = sumS . stream
-- {-# INLINE sum #-}
-- product :: Num a => [a] -> a
-- product = productS . stream
-- {-# INLINE product #-}
-- and :: [Bool] -> Bool
-- and = andS . stream
-- {-# INLINE and #-}
-- or :: [Bool] -> Bool
-- or = orS . stream
-- {-# INLINE or #-}
-- any :: (a -> Bool) -> [a] -> Bool
-- any p = anyS p . stream
-- {-# INLINE any #-}
-- all :: (a -> Bool) -> [a] -> Bool
-- all p = allS p . stream
-- {-# INLINE all #-}
--
-- foldl1' :: (a -> a -> a) -> [a] -> a
-- foldl1' f = foldl1S' f . stream
-- {-# INLINE foldl1' #-}
-- concat :: [[a]] -> [a]
-- concat = unstream . concatS . stream
-- {-# INLINE concat #-}
-- concatMap :: (a -> [b]) -> [a] -> [b]
-- concatMap f = unstream . concatMapS (stream . f) . stream
-- {-# INLINE concatMap #-}
--
-- filter :: (a -> Bool) -> [a] -> [a]
-- filter p = unstream . filterS p . stream
-- {-# INLINE filter #-}
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- lookup x = lookupS x . stream
-- {-# INLINE lookup #-}
--
-- head :: [a] -> a
-- head = headS . stream
-- {-# INLINE head #-}
-- last :: [a] -> a
-- last = lastS . stream
-- {-# INLINE last #-}
-- tail :: [a] -> [a]
-- -- destoys sharing:
-- -- tail = unstream . tailS . stream
-- tail [] = errorEmptyList "tail"
-- tail (_:xs) = xs
-- {-# INLINE tail #-}
-- init :: [a] -> [a]
-- init = unstream . initS . stream
-- {-# INLINE init #-}
-- uncons :: [a] -> Maybe (a, [a])
-- uncons [] = Nothing
-- uncons (x:xs) = Just (x, xs)
-- {-# NOINLINE [1] uncons #-}
-- -- {-# RULES "uncons/stream" forall s. uncons (unstream s) = unconsS s #-}
-- -- {-# RULES "uncons/stream" forall s. uncons (cheapUnstream s) = unconsS s #-}
-- unsnoc :: [a] -> Maybe ([a], a)
-- unsnoc = unsnocS . stream
-- {-# INLINE unsnoc #-}
-- (!?) :: [a] -> Int -> Maybe a
-- xs !? i = indexS (stream xs) i
-- {-# INLINE (!?) #-}
-- (!!) :: [a] -> Int -> a
-- xs !! i = unsafeIndexS (stream xs) i
-- {-# INLINE (!!) #-}
--
-- scanl :: (b -> a -> b) -> b -> [a] -> [b]
-- scanl k z = unstream . scanlS k z . stream
-- {-# INLINE scanl #-}
-- scanl1 :: (a -> a -> a) -> [a] -> [a]
-- scanl1 k = unstream . scanl1S k . stream
-- {-# INLINE scanl1 #-}
-- scanl' :: (b -> a -> b) -> b -> [a] -> [b]
-- scanl' k z = unstream . scanlS' k z . stream
-- {-# INLINE scanl' #-}
-- scanr :: (a -> b -> b) -> b -> [a] -> [b]
-- scanr f q0 = foldrS (\x qs@(q:_) -> f x q : qs) [q0] . stream
-- {-# INLINE scanr #-}
-- scanr1 :: (a -> a -> a) -> [a] -> [a]
-- scanr1 f = foldrS (\x qs -> case qs of [] -> [x]; (q:_) -> f x q : qs) [] . stream
-- {-# INLINE scanr1 #-}
--
-- iterate :: (a -> a) -> a -> [a]
-- iterate f x = cheapUnstream (iterateS f x)
-- {-# INLINE iterate #-}
-- iterate' :: (a -> a) -> a -> [a]
-- iterate' f x = unstream (iterateS' f x)
-- {-# INLINE iterate' #-}
-- repeat :: a -> [a]
-- repeat x = let xs = x : xs in xs
-- -- repeat = cheapUnstream . repeatS
-- {-# INLINE repeat #-}
-- replicate :: Int -> a -> [a]
-- replicate n x = cheapUnstream (replicateS n x)
-- {-# INLINE replicate #-}
-- cycle :: [a] -> [a]
-- cycle xs = ys where ys = foldrS (:) ys (stream xs)
-- -- cycle = unstream . cycleS . stream
-- {-# INLINE cycle #-}
--
-- take :: Int -> [a] -> [a]
-- take n = unstream . takeS n . stream
-- {-# INLINE take #-}
-- drop :: Int -> [a] -> [a]
-- drop 0 xs = xs
-- drop _ [] = []
-- drop n (_:xs) = drop (n - 1) xs
-- {-# NOINLINE [1] drop #-}
-- -- {-# RULES "drop/stream" forall n s. drop n (unstream s) = unstream (dropS n s) #-}
-- -- {-# RULES "drop/stream" forall n s. drop n (cheapUnstream s) = cheapUnstream (dropS n s) #-}
--
-- splitAt :: Int -> [a] -> ([a], [a])
-- splitAt 0 xs = ([], xs)
-- splitAt _ [] = ([],[])
-- splitAt n (x:xs) = let (xs',ys') = splitAt (n - 1) xs in (x : xs', ys')
-- {-# NOINLINE [1] splitAt #-}
-- -- {-# RULES "splitAt/stream" forall n s. splitAt n (unstream s) = splitAtS n s #-}
-- -- {-# RULES "splitAt/stream" forall n s. splitAt n (cheapUnstream s) = cheapSplitAtS n s #-}
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- takeWhile p = unstream . takeWhileS p . stream
-- {-# INLINE takeWhile #-}
-- dropWhile :: (a -> Bool) -> [a] -> [a]
-- dropWhile _ [] = []
-- dropWhile p xs@(x:xs')
--   | p x = dropWhile p xs'
--   | otherwise = xs
-- {-# NOINLINE [1] dropWhile #-}
-- -- {-# RULES "dropWhile/stream" forall p s. dropWhile p (unstream s) = unstream (dropWhileS p s) #-}
-- -- {-# RULES "dropWhile/stream" forall p s. dropWhile p (cheapUnstream s) = unstream (dropWhileS p s) #-}
-- span :: (a -> Bool) -> [a] -> ([a], [a])
-- -- destroys sharing and duplicates work:
-- -- span p xs = (unstream (takeWhileS p (stream xs)), unstream (dropWhileS p (stream xs)))
-- span _ [] = ([], [])
-- span p xs@(x:xs')
--   | p x = let (ys,zs) = span p xs' in (x:ys,zs)
--   | otherwise = ([], xs)
-- {-# NOINLINE [1] span #-}
-- -- {-# RULES "span/stream" forall p s. span p (unstream s) = spanS p s #-}
-- -- {-# RULES "span/stream" forall p s. span p (cheapUnstream s) = spanS p s #-}
-- break :: (a -> Bool) -> [a] -> ([a], [a])
-- break p = span (not . p)
-- {-# INLINE break #-}
-- reverse :: [a] -> [a]
-- reverse = reverseS . stream
-- {-# INLINE reverse #-}
--
-- zip :: [a] -> [b] -> [(a,b)]
-- zip xs ys = unstream (zipS (stream xs) (stream ys))
-- {-# INLINE zip #-}
-- zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
-- zip3 xs ys zs = unstream (zip3S (stream xs) (stream ys) (stream zs))
-- {-# INLINE zip3 #-}
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith f xs ys = unstream (zipWithS f (stream xs) (stream ys))
-- {-# INLINE zipWith #-}
-- zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
-- zipWith3 f xs ys zs = unstream (zipWith3S f (stream xs) (stream ys) (stream zs))
-- {-# INLINE zipWith3 #-}
--
-- unzip :: [(a,b)] -> ([a], [b])
-- unzip = unzipS . stream
-- {-# INLINE unzip #-}
-- unzip3 :: [(a,b,c)] -> ([a], [b], [c])
-- unzip3 = unzip3S . stream
-- {-# INLINE unzip3 #-}


-- $setup
-- >>> import GHC.Internal.Base
-- >>> import Prelude (Num (..), Ord (..), Int, Double, odd, not, undefined)
-- >>> import Control.DeepSeq (force)
--
-- -- compiled versions are uninterruptible.
-- https://gitlab.haskell.org/ghc/ghc/-/issues/367
--
-- >>> let or  = foldr (||) False
-- >>> let and = foldr (&&) True

--------------------------------------------------------------
-- List-manipulation functions
--------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Extract the first element of a list, which must be non-empty.
--
-- To disable the warning about partiality put @{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}@
-- at the top of the file. To disable it throughout a package put the same
-- options into @ghc-options@ section of Cabal file. To disable it in GHCi
-- put @:set -Wno-x-partial -Wno-unrecognised-warning-flags@ into @~/.ghci@ config file.
-- See also the [migration guide](https://github.com/haskell/core-libraries-committee/blob/main/guides/warning-for-head-and-tail.md).
--
-- ===== __Examples__
--
-- >>> head [1, 2, 3]
-- 1
--
-- >>> head [1..]
-- 1
--
-- >>> head []
-- *** Exception: Prelude.head: empty list
head                    :: HasCallStack => [a] -> a
head (x:_)              =  x
head []                 =  badHead
{-# NOINLINE [1] head #-}

{-# WARNING in "x-partial" head "This is a partial function, it throws an error on empty lists. Use pattern matching, 'Data.List.uncons' or 'Data.Maybe.listToMaybe' instead. Consider refactoring to use \"Data.List.NonEmpty\"." #-}

badHead :: HasCallStack => a
badHead = errorEmptyList "head"

-- This rule is useful in cases like
--      head [y | (x,y) <- ps, x==t]
{-# RULES
"head/unstream" forall s. head (unstream s) = headS s
"head/cheapUnstream" forall s. head (cheapUnstream s) = headS s
-- "head/build"    forall (g::forall b.(a->b->b)->b->b) .
--                 head (build g) = g (\x _ -> x) badHead
-- "head/augment"  forall xs (g::forall b. (a->b->b) -> b -> b) .
--                 head (augment g xs) = g (\x _ -> x) (head xs)
 #-}

-- | \(\mathcal{O}(1)\). Decompose a list into its 'head' and 'tail'.
--
-- * If the list is empty, returns 'Nothing'.
-- * If the list is non-empty, returns @'Just' (x, xs)@,
-- where @x@ is the 'head' of the list and @xs@ its 'tail'.
--
-- @since base-4.8.0.0
--
-- ==== __Examples__
--
-- >>> uncons []
-- Nothing
--
-- >>> uncons [1]
-- Just (1,[])
--
-- >>> uncons [1, 2, 3]
-- Just (1,[2,3])
uncons                  :: [a] -> Maybe (a, [a])
uncons []               = Nothing
uncons (x:xs)           = Just (x, xs)

-- | \(\mathcal{O}(n)\). Decompose a list into 'init' and 'last'.
--
-- * If the list is empty, returns 'Nothing'.
-- * If the list is non-empty, returns @'Just' (xs, x)@,
-- where @xs@ is the 'init'ial part of the list and @x@ is its 'last' element.
--
--
-- 'unsnoc' is dual to 'uncons': for a finite list @xs@
--
-- > unsnoc xs = (\(hd, tl) -> (reverse tl, hd)) <$> uncons (reverse xs)
--
-- ==== __Examples__
--
-- >>> unsnoc []
-- Nothing
--
-- >>> unsnoc [1]
-- Just ([],1)
--
-- >>> unsnoc [1, 2, 3]
-- Just ([1,2],3)
--
-- ==== __Laziness__
--
-- >>> fst <$> unsnoc [undefined]
-- Just []
--
-- >>> head . fst <$> unsnoc (1 : undefined)
-- Just *** Exception: Prelude.undefined
--
-- >>> head . fst <$> unsnoc (1 : 2 : undefined)
-- Just 1
--
-- @since base-4.19.0.0
unsnoc :: [a] -> Maybe ([a], a)
-- The lazy pattern ~(a, b) is important to be productive on infinite lists
-- and not to be prone to stack overflows.
-- Expressing the recursion via 'foldr' provides for list fusion.
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
{-# INLINABLE unsnoc #-}

-- | \(\mathcal{O}(1)\). Extract the elements after the head of a list, which
-- must be non-empty.
--
-- To disable the warning about partiality put @{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}@
-- at the top of the file. To disable it throughout a package put the same
-- options into @ghc-options@ section of Cabal file. To disable it in GHCi
-- put @:set -Wno-x-partial -Wno-unrecognised-warning-flags@ into @~/.ghci@ config file.
-- See also the [migration guide](https://github.com/haskell/core-libraries-committee/blob/main/guides/warning-for-head-and-tail.md).
--
-- ==== __Examples__
--
-- >>> tail [1, 2, 3]
-- [2,3]
--
-- >>> tail [1]
-- []
--
-- >>> tail []
-- *** Exception: Prelude.tail: empty list
tail                    :: HasCallStack => [a] -> [a]
tail (_:xs)             =  xs
tail []                 =  errorEmptyList "tail"

{-# WARNING in "x-partial" tail "This is a partial function, it throws an error on empty lists. Replace it with 'drop' 1, or use pattern matching or 'GHC.Internal.Data.List.uncons' instead. Consider refactoring to use \"Data.List.NonEmpty\"." #-}

-- | \(\mathcal{O}(n)\). Extract the last element of a list, which must be
-- finite and non-empty.
--
-- WARNING: This function is partial. Consider using 'unsnoc' instead.
--
-- ==== __Examples__
--
-- >>> last [1, 2, 3]
-- 3
--
-- >>> last [1..]
-- * Hangs forever *
--
-- >>> last []
-- *** Exception: Prelude.last: empty list
last                    :: HasCallStack => [a] -> a
#if defined(USE_REPORT_PRELUDE)
last [x]                =  x
last (_:xs)             =  last xs
last []                 =  errorEmptyList "last"
#else
-- Use foldl to make last a good consumer.
-- This will compile to good code for the actual GHC.Internal.List.last.
-- (At least as long it is eta-expanded, otherwise it does not, #10260.)
last xs = foldl (\_ x -> x) lastError xs
{-# INLINE last #-}
-- The inline pragma is required to make GHC remember the implementation via
-- foldl.
lastError :: HasCallStack => a
lastError = errorEmptyList "last"
#endif

-- | \(\mathcal{O}(n)\). Return all the elements of a list except the last one.
-- The list must be non-empty.
--
-- WARNING: This function is partial. Consider using 'unsnoc' instead.
--
-- ==== __Examples__
--
-- >>> init [1, 2, 3]
-- [1,2]
--
-- >>> init [1]
-- []
--
-- >>> init []
-- *** Exception: Prelude.init: empty list
init                    :: HasCallStack => [a] -> [a]
#if defined(USE_REPORT_PRELUDE)
init [x]                =  []
init (x:xs)             =  x : init xs
init []                 =  errorEmptyList "init"
#else
-- eliminate repeated cases
init []                 =  errorEmptyList "init"
init (x:xs)             =  init' x xs
  where init' _ []     = []
        init' y (z:zs) = y : init' z zs
#endif

-- | \(\mathcal{O}(1)\). Test whether a list is empty.
--
-- >>> null []
-- True
-- >>> null [1]
-- False
-- >>> null [1..]
-- False
null                    :: [a] -> Bool
null []                 =  True
null (_:_)              =  False

-- | \(\mathcal{O}(n)\). 'length' returns the length of a finite list as an
-- 'Int'. It is an instance of the more general 'GHC.Internal.Data.List.genericLength', the
-- result type of which may be any kind of number.
--
-- >>> length []
-- 0
-- >>> length ['a', 'b', 'c']
-- 3
-- >>> length [1..]
-- * Hangs forever *
{-# NOINLINE [1] length #-}
length                  :: [a] -> Int
length xs               = lenAcc xs 0

lenAcc          :: [a] -> Int -> Int
lenAcc []     n = n
lenAcc (_:ys) n = lenAcc ys (n+1)

{-# RULES
"length" length = lengthS . stream
-- "length" [~1] forall xs . length xs = foldr lengthFB idLength xs 0
-- "lengthList" [1] foldr lengthFB idLength = lenAcc
 #-}

-- The lambda form turns out to be necessary to make this inline
-- when we need it to and give good performance.
{-# INLINE [0] lengthFB #-}
lengthFB :: x -> (Int -> Int) -> Int -> Int
lengthFB _ r = \ !a -> r (a + 1)

{-# INLINE [0] idLength #-}
idLength :: Int -> Int
idLength = id

-- | \(\mathcal{O}(n)\). 'filter', applied to a predicate and a list, returns
-- the list of those elements that satisfy the predicate; i.e.,
--
-- > filter p xs = [ x | x <- xs, p x]
--
-- ==== __Examples__
--
-- >>> filter odd [1, 2, 3]
-- [1,3]
--
-- >>> filter (\l -> length l > 3) ["Hello", ", ", "World", "!"]
-- ["Hello","World"]
--
-- >>> filter (/= 3) [1, 2, 3, 4, 3, 2, 1]
-- [1,2,4,2,1]
{-# NOINLINE [1] filter #-}
filter :: (a -> Bool) -> [a] -> [a]
filter _pred []    = []
filter pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      = filter pred xs

{-# INLINE [0] filterFB #-} -- See Note [Inline FB functions]
filterFB :: (a -> b -> b) -> (a -> Bool) -> a -> b -> b
filterFB c p x r | p x       = x `c` r
                 | otherwise = r

{-# RULES
"filter" forall p. filter p = unstream . filterS p . stream
-- "filter"     [~1] forall p xs.  filter p xs = build (\c n -> foldr (filterFB c p) n xs)
-- "filterList" [1]  forall p.     foldr (filterFB (:) p) [] = filter p
-- "filterFB"        forall c p q. filterFB (filterFB c p) q = filterFB c (\x -> q x && p x)
 #-}

-- Note the filterFB rule, which has p and q the "wrong way round" in the RHS.
--     filterFB (filterFB c p) q a b
--   = if q a then filterFB c p a b else b
--   = if q a then (if p a then c a b else b) else b
--   = if q a && p a then c a b else b
--   = filterFB c (\x -> q x && p x) a b
-- I originally wrote (\x -> p x && q x), which is wrong, and actually
-- gave rise to a live bug report.  SLPJ.


-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a list, reduces the list
-- using the binary operator, from left to right:
--
-- > foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
--
-- The list must be finite.
--
-- >>> foldl (+) 0 [1..4]
-- 10
-- >>> foldl (+) 42 []
-- 42
-- >>> foldl (-) 100 [1..4]
-- 90
-- >>> foldl (\reversedString nextChar -> nextChar : reversedString) "foo" ['a', 'b', 'c', 'd']
-- "dcbafoo"
-- >>> foldl (+) 0 [1..]
-- * Hangs forever *
foldl :: forall a b. (b -> a -> b) -> b -> [a] -> b
foldl k z0 [] = z0
foldl k z0 (x:xs) = foldl k (k z0 x) xs
{-# NOINLINE [0] foldl #-}

{-# RULES
"foldl" forall k z . foldl k z = foldlS k z . stream
 #-}

{-
Note [Left folds via right fold]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Implementing foldl et. al. via foldr is only a good idea if the compiler can
optimize the resulting code (eta-expand the recursive "go"). See #7994.
We hope that one of the two measure kick in:

   * Call Arity (-fcall-arity, enabled by default) eta-expands it if it can see
     all calls and determine that the arity is large.
   * The oneShot annotation gives a hint to the regular arity analysis that
     it may assume that the lambda is called at most once.
     See [One-shot lambdas] in CoreArity and especially [Eta expanding thunks]
     in CoreArity.

The oneShot annotations used in this module are correct, as we only use them in
arguments to foldr, where we know how the arguments are called.

Note [Inline FB functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~
After fusion rules successfully fire, we are usually left with one or more calls
to list-producing functions abstracted over cons and nil. Here we call them
FB functions because their names usually end with 'FB'. It's a good idea to
inline FB functions because:

* They are higher-order functions and therefore benefit from inlining.

* When the final consumer is a left fold, inlining the FB functions is the only
  way to make arity expansion happen. See Note [Left folds via right fold].

For this reason we mark all FB functions INLINE [0]. The [0] phase-specifier
ensures that calls to FB functions can be written back to the original form
when no fusion happens.

Without these inline pragmas, the loop in perf/should_run/T13001 won't be
allocation-free. Also see #13001.
-}

-- ----------------------------------------------------------------------------

-- | A strict version of 'foldl'.
foldl' :: forall a b . (b -> a -> b) -> b -> [a] -> b
foldl' k !z0 [] = z0
foldl' k !z0 (x:xs) = foldl' k (k z0 x) xs

{-# NOINLINE [0] foldl' #-}
{-# RULES
"foldl'" forall k z. foldl' k z = foldlS' k z . stream
 #-}

{-
Note [Definition of foldl']
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want foldl' to be a good consumer, so:

* We define it (rather cunningly) with `foldr`.  That way, the `fold/build`
  rule might fire.  See Note [Left folds via right fold]

* We give it an INLINE pragma, so that it'll inline at its call sites, again
  giving the `fold/build` rule a chance to fire.

* We eta-reduce it so that it has arity 2, not 3.  Reason: consider

     sumlen :: [Float] -> (Float, Int)
     sumlen = foldl' f (0, 0)
        where
          f (!s, !n) !x = (s + x, n + 1)

The RHS of `sumlen` is a partial application of foldl', and is not
eta-expanded (and it isn't, because we don't eta-expand PAPs.  See Note
[Do not eta-expand PAPs] in GHC.Core.Opt.Simplify.Utils)

So foldl' is partially applied to two arguments, /and it won't inline/
if its defn is:

      {-# INLINE foldl' #-}
      foldl' k z xs = ...

because INLINE functions only inline when saturated.

Conclusion: move the `xs` parameter to the RHS, and define it thus

  fold' k z = \xs -> ...

See !5259 for additional discussion.  This may result in partial applications
of 'foldl'' inlining in some functions where they previously did not.  Absent
an INLINE pragam for the calling function, it may become too expensive to
automatically inline, resulting in a loss of previously accidental list
fusion.  Such call sites may now need explicit INLINE or INLINABLE pragmas
to make the desired list fusion robust.
-}

-- | 'foldl1' is a variant of 'foldl' that has no starting value argument,
-- and thus must be applied to non-empty lists. Note that unlike 'foldl', the accumulated value must be of the same type as the list elements.
--
-- >>> foldl1 (+) [1..4]
-- 10
-- >>> foldl1 (+) []
-- *** Exception: Prelude.foldl1: empty list
-- >>> foldl1 (-) [1..4]
-- -8
-- >>> foldl1 (&&) [True, False, True, True]
-- False
-- >>> foldl1 (||) [False, False, True, True]
-- True
-- >>> foldl1 (+) [1..]
-- * Hangs forever *
foldl1                  :: HasCallStack => (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)         =  foldl f x xs
foldl1 _ []             =  errorEmptyList "foldl1"

-- | A strict version of 'foldl1'.
foldl1'                  :: HasCallStack => (a -> a -> a) -> [a] -> a
foldl1' f (x:xs)         =  foldl' f x xs
foldl1' _ []             =  errorEmptyList "foldl1'"

-- -----------------------------------------------------------------------------
-- List sum and product

-- | The 'sum' function computes the sum of a finite list of numbers.
--
-- >>> sum []
-- 0
-- >>> sum [42]
-- 42
-- >>> sum [1..10]
-- 55
-- >>> sum [4.1, 2.0, 1.7]
-- 7.8
-- >>> sum [1..]
-- * Hangs forever *
sum                     :: (Num a) => [a] -> a
{-# INLINE sum #-}
sum                     =  foldl' (+) 0

-- | The 'product' function computes the product of a finite list of numbers.
--
-- >>> product []
-- 1
-- >>> product [42]
-- 42
-- >>> product [1..10]
-- 3628800
-- >>> product [4.1, 2.0, 1.7]
-- 13.939999999999998
-- >>> product [1..]
-- * Hangs forever *
product                 :: (Num a) => [a] -> a
{-# INLINE product #-}
product                 =  foldl' (*) 1

-- | \(\mathcal{O}(n)\). 'scanl' is similar to 'foldl', but returns a list of
-- successive reduced values from the left:
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs
--
-- ==== __Examples__
--
-- >>> scanl (+) 0 [1..4]
-- [0,1,3,6,10]
--
-- >>> scanl (+) 42 []
-- [42]
--
-- >>> scanl (-) 100 [1..4]
-- [100,99,97,94,90]
--
-- >>> scanl (\reversedString nextChar -> nextChar : reversedString) "foo" ['a', 'b', 'c', 'd']
-- ["foo","afoo","bafoo","cbafoo","dcbafoo"]
--
-- >>> take 10 (scanl (+) 0 [1..])
-- [0,1,3,6,10,15,21,28,36,45]
--
-- >>> take 1 (scanl undefined 'a' undefined)
-- "a"

-- This peculiar arrangement is necessary to prevent scanl being rewritten in
-- its own right-hand side.
{-# NOINLINE [1] scanl #-}
scanl                   :: (b -> a -> b) -> b -> [a] -> [b]
scanl                   = scanlGo
  where
    scanlGo           :: (b -> a -> b) -> b -> [a] -> [b]
    scanlGo f q ls    = q : (case ls of
                               []   -> []
                               x:xs -> scanlGo f (f q x) xs)

-- See Note [scanl rewrite rules]
{-# RULES
"scanl" forall f a. scanl f a = unstream . scanlS f a . stream
-- "scanl"  [~1] forall f a bs . scanl f a bs =
--   build (\c n -> a `c` foldr (scanlFB f c) (constScanl n) bs a)
-- "scanlList" [1] forall f (a::a) bs .
--     foldr (scanlFB f (:)) (constScanl []) bs a = tail (scanl f a bs)
 #-}

{-# INLINE [0] scanlFB #-} -- See Note [Inline FB functions]
scanlFB :: (b -> a -> b) -> (b -> c -> c) -> a -> (b -> c) -> b -> c
scanlFB f c = \b g -> oneShot (\x -> let b' = f x b in b' `c` g b')
  -- See Note [Left folds via right fold]

{-# INLINE [0] constScanl #-}
constScanl :: a -> b -> a
constScanl = const


-- | \(\mathcal{O}(n)\). 'scanl1' is a variant of 'scanl' that has no starting
-- value argument:
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
--
-- ==== __Examples__
--
-- >>> scanl1 (+) [1..4]
-- [1,3,6,10]
--
-- >>> scanl1 (+) []
-- []
--
-- >>> scanl1 (-) [1..4]
-- [1,-1,-4,-8]
--
-- >>> scanl1 (&&) [True, False, True, True]
-- [True,False,False,False]
--
-- >>> scanl1 (||) [False, False, True, True]
-- [False,False,True,True]
--
-- >>> take 10 (scanl1 (+) [1..])
-- [1,3,6,10,15,21,28,36,45,55]
--
-- >>> take 1 (scanl1 undefined ('a' : undefined))
-- "a"
scanl1                  :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)         =  scanl f x xs
scanl1 _ []             =  []

-- | \(\mathcal{O}(n)\). A strict version of 'scanl'.
{-# NOINLINE [1] scanl' #-}
scanl'           :: (b -> a -> b) -> b -> [a] -> [b]
-- This peculiar form is needed to prevent scanl' from being rewritten
-- in its own right hand side.
scanl' = scanlGo'
  where
    scanlGo'           :: (b -> a -> b) -> b -> [a] -> [b]
    scanlGo' f !q ls    = q : (case ls of
                            []   -> []
                            x:xs -> scanlGo' f (f q x) xs)

-- See Note [scanl rewrite rules]
{-# RULES
"scanl'" forall f a. scanl' f a = unstream . scanlS' f a . stream
-- "scanl'"  [~1] forall f a bs . scanl' f a bs =
--   build (\c n -> a `c` foldr (scanlFB' f c) (flipSeq n) bs a)
-- "scanlList'" [1] forall f a bs .
--     foldr (scanlFB' f (:)) (flipSeq []) bs a = tail (scanl' f a bs)
 #-}

{-# INLINE [0] scanlFB' #-} -- See Note [Inline FB functions]
scanlFB' :: (b -> a -> b) -> (b -> c -> c) -> a -> (b -> c) -> b -> c
scanlFB' f c = \b g -> oneShot (\x -> let !b' = f x b in b' `c` g b')
  -- See Note [Left folds via right fold]

{-
Note [scanl rewrite rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~
In most cases, when we rewrite a form to one that can fuse, we try to rewrite it
back to the original form if it does not fuse. For scanl, we do something a
little different. In particular, we rewrite

scanl f a bs

to

build (\c n -> a `c` foldr (scanlFB f c) (constScanl n) bs a)

When build is inlined, this becomes

a : foldr (scanlFB f (:)) (constScanl []) bs a

To rewrite this form back to scanl, we would need a rule that looked like

forall f a bs. a : foldr (scanlFB f (:)) (constScanl []) bs a = scanl f a bs

The problem with this rule is that it has (:) at its head. This would have the
effect of changing the way the inliner looks at (:), not only here but
everywhere.  In most cases, this makes no difference, but in some cases it
causes it to come to a different decision about whether to inline something.
Based on nofib benchmarks, this is bad for performance. Therefore, we instead
match on everything past the :, which is just the tail of scanl.
-}

-- foldr, foldr', foldr1, scanr, and scanr1 are the right-to-left duals of the
-- above functions.

-- | 'foldr'' is a variant of 'foldr' that begins list reduction from the last
-- element and evaluates the accumulator strictly as it unwinds the stack back
-- to the beginning of the list.  The input list /must/ be finite, otherwise
-- 'foldr'' runs out of space (/diverges/).
--
-- Note that if the function that combines the accumulated value with each
-- element is strict in the accumulator, other than a possible improvement
-- in the constant factor, you get the same \(\mathcal{O}(n)\) space cost
-- as with just 'foldr'.
--
-- If you want a strict right fold in constant space, you need a structure
-- that supports faster than \(\mathcal{O}(n)\) access to the right-most
-- element, such as @Seq@ from the @containers@ package.
--
-- Use of this function is a hint that the @[]@ structure may be a poor fit
-- for the task at hand.  If the order in which the elements are combined is
-- not important, use 'foldl'' instead.
--
-- >>> foldr' (+) [1..4]  -- Use foldl' instead!
-- 10
-- >>> foldr' (&&) [True, False, True, True] -- Use foldr instead!
-- False
-- >>> foldr' (||) [False, False, True, True] -- Use foldr instead!
-- True
foldr' :: (a -> b -> b) -> b -> [a] -> b
{-# INLINE foldr' #-}
foldr' f z0 xs = foldl f' id xs z0
  where f' k x z = k $! f x z

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty lists. Note that unlike 'foldr', the accumulated value must be of the same type as the list elements.
--
-- >>> foldr1 (+) [1..4]
-- 10
-- >>> foldr1 (+) []
-- *** Exception: Prelude.foldr1: empty list
-- >>> foldr1 (-) [1..4]
-- -2
-- >>> foldr1 (&&) [True, False, True, True]
-- False
-- >>> foldr1 (||) [False, False, True, True]
-- True
-- >>> force $ foldr1 (+) [1..]
-- *** Exception: stack overflow
foldr1                  :: HasCallStack => (a -> a -> a) -> [a] -> a
foldr1 f = go
  where go [x]            =  x
        go (x:xs)         =  f x (go xs)
        go []             =  errorEmptyList "foldr1"
{-# INLINE [0] foldr1 #-}
{-# RULES
"foldr1" forall f . foldr1 f = foldr1S f . stream
 #-}

-- | \(\mathcal{O}(n)\). 'scanr' is the right-to-left dual of 'scanl'. Note that the order of parameters on the accumulating function are reversed compared to 'scanl'.
-- Also note that
--
-- > head (scanr f z xs) == foldr f z xs.
--
-- ==== __Examples__
--
-- >>> scanr (+) 0 [1..4]
-- [10,9,7,4,0]
--
-- >>> scanr (+) 42 []
-- [42]
--
-- >>> scanr (-) 100 [1..4]
-- [98,-97,99,-96,100]
--
-- >>> scanr (\nextChar reversedString -> nextChar : reversedString) "foo" ['a', 'b', 'c', 'd']
-- ["abcdfoo","bcdfoo","cdfoo","dfoo","foo"]
--
-- >>> force $ scanr (+) 0 [1..]
-- *** Exception: stack overflow
{-# NOINLINE [1] scanr #-}
scanr                   :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ q0 []           =  [q0]
scanr f q0 (x:xs)       =  f x q : qs
                           where qs@(q:_) = scanr f q0 xs

{-# INLINE [0] strictUncurryScanr #-}
strictUncurryScanr :: (a -> b -> c) -> (a, b) -> c
strictUncurryScanr f pair = case pair of
                              (x, y) -> f x y

{-# INLINE [0] scanrFB #-} -- See Note [Inline FB functions]
scanrFB :: (a -> b -> b) -> (b -> c -> c) -> a -> (b, c) -> (b, c)
scanrFB f c = \x ~(r, est) -> (f x r, r `c` est)
-- This lazy pattern match on the tuple is necessary to prevent
-- an infinite loop when scanr receives a fusable infinite list,
-- which was the reason for #16943.
-- See Note [scanrFB and evaluation] below

{-# RULES
"scanr" forall f q0. scanr f q0 = scanrS f q0 . stream
-- "scanr" [~1] forall f q0 ls . scanr f q0 ls =
--   build (\c n -> strictUncurryScanr c (foldr (scanrFB f c) (q0,n) ls))
-- "scanrList" [1] forall f q0 ls .
--                strictUncurryScanr (:) (foldr (scanrFB f (:)) (q0,[]) ls) =
--                  scanr f q0 ls
 #-}

{-
Note [scanrFB and evaluation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a previous Version, the pattern match on the tuple in scanrFB used to be
strict. If scanr is called with a build expression, the following would happen:
The rule "scanr" would fire, and we obtain
    build (\c n -> strictUncurryScanr c (foldr (scanrFB f c) (q0,n) (build g))))
The rule "foldr/build" now fires, and the second argument of strictUncurryScanr
will be the expression
    g (scanrFB f c) (q0,n)
which will be evaluated, thanks to strictUncurryScanr.
The type of (g :: (a -> b -> b) -> b -> b) allows us to apply parametricity:
Either the tuple is returned (trivial), or scanrFB is called:
    g (scanrFB f c) (q0,n) = scanrFB ... (g' (scanrFB f c) (q0,n))
Notice that thanks to the strictness of scanrFB, the expression
g' (scanrFB f c) (q0,n) gets evaluated as well. In particular, if g' is a
recursive case of g, parametricity applies again and we will again have a
possible call to scanrFB. In short, g (scanrFB f c) (q0,n) will end up being
completely evaluated. This is resource consuming for large lists and if the
recursion has no exit condition (and this will be the case in functions like
repeat or cycle), the program will crash (see #16943).
The solution: Don't make scanrFB strict in its last argument. Doing so will
remove the cause for the chain of evaluations, and all is well.
-}

-- | \(\mathcal{O}(n)\). 'scanr1' is a variant of 'scanr' that has no starting
-- value argument.
--
-- ==== __Examples__
--
-- >>> scanr1 (+) [1..4]
-- [10,9,7,4]
--
-- >>> scanr1 (+) []
-- []
--
-- >>> scanr1 (-) [1..4]
-- [-2,3,-1,4]
--
-- >>> scanr1 (&&) [True, False, True, True]
-- [False,False,True,True]
--
-- >>> scanr1 (||) [True, True, False, False]
-- [True,True,False,False]
--
-- >>> force $ scanr1 (+) [1..]
-- *** Exception: stack overflow
scanr1                  :: (a -> a -> a) -> [a] -> [a]
scanr1 _ []             =  []
scanr1 _ [x]            =  [x]
scanr1 f (x:xs)         =  f x q : qs
                           where qs@(q:_) = scanr1 f xs
{-# NOINLINE [0] scanr1 #-}
{-# RULES
"scanr1" forall f. scanr1 f = scanr1S f . stream
 #-}

-- | 'maximum' returns the maximum value from a list,
-- which must be non-empty, finite, and of an ordered type.
-- It is a special case of 'GHC.Internal.Data.List.maximumBy', which allows the
-- programmer to supply their own comparison function.
--
-- >>> maximum []
-- *** Exception: Prelude.maximum: empty list
-- >>> maximum [42]
-- 42
-- >>> maximum [55, -12, 7, 0, -89]
-- 55
-- >>> maximum [1..]
-- * Hangs forever *
maximum                 :: (Ord a, HasCallStack) => [a] -> a
{-# INLINABLE maximum #-}
maximum []              =  errorEmptyList "maximum"
maximum xs              =  foldl1' max xs

-- We want this to be specialized so that with a strict max function, GHC
-- produces good code. Note that to see if this is happening, one has to
-- look at -ddump-prep, not -ddump-core!
{-# SPECIALIZE  maximum :: [Int] -> Int #-}
{-# SPECIALIZE  maximum :: [Integer] -> Integer #-}

-- | 'minimum' returns the minimum value from a list,
-- which must be non-empty, finite, and of an ordered type.
-- It is a special case of 'GHC.Internal.Data.List.minimumBy', which allows the
-- programmer to supply their own comparison function.
--
-- >>> minimum []
-- *** Exception: Prelude.minimum: empty list
-- >>> minimum [42]
-- 42
-- >>> minimum [55, -12, 7, 0, -89]
-- -89
-- >>> minimum [1..]
-- * Hangs forever *
minimum                 :: (Ord a, HasCallStack) => [a] -> a
{-# INLINABLE minimum #-}
minimum []              =  errorEmptyList "minimum"
minimum xs              =  foldl1' min xs

{-# SPECIALIZE  minimum :: [Int] -> Int #-}
{-# SPECIALIZE  minimum :: [Integer] -> Integer #-}


-- | 'iterate' @f x@ returns an infinite list of repeated applications
-- of @f@ to @x@:
--
-- > iterate f x == [x, f x, f (f x), ...]
--
-- ==== __Laziness__
--
-- Note that 'iterate' is lazy, potentially leading to thunk build-up if
-- the consumer doesn't force each iterate. See 'iterate'' for a strict
-- variant of this function.
--
-- >>> take 1 $ iterate undefined 42
-- [42]
--
-- ==== __Examples__
--
-- >>> take 10 $ iterate not True
-- [True,False,True,False,True,False,True,False,True,False]
--
-- >>> take 10 $ iterate (+3) 42
-- [42,45,48,51,54,57,60,63,66,69]
--
-- @iterate id == 'repeat'@:
--
-- >>> take 10 $ iterate id 1
-- [1,1,1,1,1,1,1,1,1,1]
{-# NOINLINE [1] iterate #-}
iterate :: (a -> a) -> a -> [a]
iterate f x =  x : iterate f (f x)

{-# INLINE [0] iterateFB #-} -- See Note [Inline FB functions]
iterateFB :: (a -> b -> b) -> (a -> a) -> a -> b
iterateFB c f x0 = go x0
  where go x = x `c` go (f x)

{-# RULES
"iterate" forall f x. iterate f x = unstream (iterateS f x)
-- "iterate"    [~1] forall f x.   iterate f x = build (\c _n -> iterateFB c f x)
-- "iterateFB"  [1]                iterateFB (:) = iterate
 #-}


-- | 'iterate'' is the strict version of 'iterate'.
--
-- It forces the result of each application of the function to weak head normal
-- form (WHNF)
-- before proceeding.
--
-- >>> take 1 $ iterate' undefined 42
-- *** Exception: Prelude.undefined
{-# NOINLINE [1] iterate' #-}
iterate' :: (a -> a) -> a -> [a]
iterate' f x =
    let x' = f x
    in x' `seq` (x : iterate' f x')

{-# INLINE [0] iterate'FB #-} -- See Note [Inline FB functions]
iterate'FB :: (a -> b -> b) -> (a -> a) -> a -> b
iterate'FB c f x0 = go x0
  where go x =
            let x' = f x
            in x' `seq` (x `c` go x')

{-# RULES
"iterate'" forall f x. iterate' f x = unstream (iterateS' f x)
-- "iterate'"    [~1] forall f x.   iterate' f x = build (\c _n -> iterate'FB c f x)
-- "iterate'FB"  [1]                iterate'FB (:) = iterate'
 #-}


-- | 'repeat' @x@ is an infinite list, with @x@ the value of every element.
--
-- ==== __Examples__
--
-- >>> take 10 $ repeat 17
-- [17,17,17,17,17,17,17,17,17, 17]
--
-- >>> repeat undefined
-- [*** Exception: Prelude.undefined
repeat :: a -> [a]
{-# INLINE [0] repeat #-}
-- The pragma just gives the rules more chance to fire
repeat x = xs where xs = x : xs

{-# INLINE [0] repeatFB #-}     -- ditto -- See Note [Inline FB functions]
repeatFB :: (a -> b -> b) -> a -> b
repeatFB c x = xs where xs = x `c` xs


-- {-# RULES
-- "repeat"    [~1] forall x. repeat x = build (\c _n -> repeatFB c x)
-- "repeatFB"  [1]  repeatFB (:)       = repeat
--  #-}

-- | 'replicate' @n x@ is a list of length @n@ with @x@ the value of
-- every element.
-- It is an instance of the more general 'GHC.Internal.Data.List.genericReplicate',
-- in which @n@ may be of any integral type.
--
-- ==== __Examples__
--
-- >>> replicate 0 True
-- []
--
-- >>> replicate (-1) True
-- []
--
-- >>> replicate 4 True
-- [True,True,True,True]
{-# NOINLINE [0] replicate #-}
replicate               :: Int -> a -> [a]
replicate n x           =  take n (repeat x)
{-# RULES
"replicate" forall n x . replicate n x = cheapUnstream (replicateS n x)
#-}

-- | 'cycle' ties a finite list into a circular one, or equivalently,
-- the infinite repetition of the original list.  It is the identity
-- on infinite lists.
--
-- ==== __Examples__
--
-- >>> cycle []
-- *** Exception: Prelude.cycle: empty list
--
-- >>> take 10 (cycle [42])
-- [42,42,42,42,42,42,42,42,42,42]
--
-- >>> take 10 (cycle [2, 5, 7])
-- [2,5,7,2,5,7,2,5,7,2]
--
-- >>> take 1 (cycle (42 : undefined))
-- [42]
cycle                   :: HasCallStack => [a] -> [a]
cycle []                = errorEmptyList "cycle"
cycle xs                = xs' where xs' = xs ++ xs'

-- | 'takeWhile', applied to a predicate @p@ and a list @xs@, returns the
-- longest prefix (possibly empty) of @xs@ of elements that satisfy @p@.
--
-- ==== __Laziness__
--
-- >>> takeWhile (const False) undefined
-- *** Exception: Prelude.undefined
--
-- >>> takeWhile (const False) (undefined : undefined)
-- []
--
-- >>> take 1 (takeWhile (const True) (1 : undefined))
-- [1]
--
-- ==== __Examples__
--
-- >>> takeWhile (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
--
-- >>> takeWhile (< 9) [1,2,3]
-- [1,2,3]
--
-- >>> takeWhile (< 0) [1,2,3]
-- []
{-# NOINLINE [1] takeWhile #-}
takeWhile               :: (a -> Bool) -> [a] -> [a]
takeWhile _ []          =  []
takeWhile p (x:xs)
            | p x       =  x : takeWhile p xs
            | otherwise =  []

{-# INLINE [0] takeWhileFB #-} -- See Note [Inline FB functions]
takeWhileFB :: (a -> Bool) -> (a -> b -> b) -> b -> a -> b -> b
takeWhileFB p c n = \x r -> if p x then x `c` r else n

-- The takeWhileFB rule is similar to the filterFB rule. It works like this:
-- takeWhileFB q (takeWhileFB p c n) n =
-- \x r -> if q x then (takeWhileFB p c n) x r else n =
-- \x r -> if q x then (\x' r' -> if p x' then x' `c` r' else n) x r else n =
-- \x r -> if q x then (if p x then x `c` r else n) else n =
-- \x r -> if q x && p x then x `c` r else n =
-- takeWhileFB (\x -> q x && p x) c n
{-# RULES
"takeWhile" forall p. takeWhile p = unstream . takeWhileS p . stream
-- "takeWhile"     [~1] forall p xs. takeWhile p xs =
--                                 build (\c n -> foldr (takeWhileFB p c n) n xs)
-- "takeWhileList" [1]  forall p.    foldr (takeWhileFB p (:) []) [] = takeWhile p
-- "takeWhileFB"        forall c n p q. takeWhileFB q (takeWhileFB p c n) n =
--                         takeWhileFB (\x -> q x && p x) c n
 #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
--
-- ==== __Examples__
--
-- >>> dropWhile (< 3) [1,2,3,4,5,1,2,3]
-- [3,4,5,1,2,3]
--
-- >>> dropWhile (< 9) [1,2,3]
-- []
--
-- >>> dropWhile (< 0) [1,2,3]
-- [1,2,3]
dropWhile               :: (a -> Bool) -> [a] -> [a]
dropWhile _ []          =  []
dropWhile p xs@(x:xs')
            | p x       =  dropWhile p xs'
            | otherwise =  xs
{-# NOINLINE [0] dropWhile #-}

{-# RULES
"dropWhile/unstream" forall p s. dropWhile p (unstream s) = unstream (dropWhileS p s)
"dropWhile/cheapUnstream" forall p s. dropWhile p (cheapUnstream s) = cheapUnstream (dropWhileS p s)
 #-}

-- | 'take' @n@, applied to a list @xs@, returns the prefix of @xs@
-- of length @n@, or @xs@ itself if @n >= 'length' xs@.
--
-- It is an instance of the more general 'GHC.Internal.Data.List.genericTake',
-- in which @n@ may be of any integral type.
--
-- ==== __Laziness__
--
-- >>> take 0 undefined
-- []
-- >>> take 2 (1 : 2 : undefined)
-- [1,2]
--
-- ==== __Examples__
--
-- >>> take 5 "Hello World!"
-- "Hello"
--
-- >>> take 3 [1,2,3,4,5]
-- [1,2,3]
--
-- >>> take 3 [1,2]
-- [1,2]
--
-- >>> take 3 []
-- []
--
-- >>> take (-1) [1,2]
-- []
--
-- >>> take 0 [1,2]
-- []
take                   :: Int -> [a] -> [a]
#if defined(USE_REPORT_PRELUDE)
take n _      | n <= 0 =  []
take _ []              =  []
take n (x:xs)          =  x : take (n-1) xs
#else

{- We always want to inline this to take advantage of a known length argument
sign. Note, however, that it's important for the RULES to grab take, rather
than trying to INLINE take immediately and then letting the RULES grab
unsafeTake. Presumably the latter approach doesn't grab it early enough; it led
to an allocation regression in nofib/fft2. -}
{-# INLINE [1] take #-}
take n xs | 0 < n     = unsafeTake n xs
          | otherwise = []

-- A version of take that takes the whole list if it's given an argument less
-- than 1.
{-# NOINLINE [0] unsafeTake #-} -- See Note [Inline FB functions]
unsafeTake :: Int -> [a] -> [a]
unsafeTake !_  []     = []
unsafeTake 1   (x: _) = [x]
unsafeTake m   (x:xs) = x : unsafeTake (m - 1) xs

{-# RULES
"take" forall n. take n = unstream . takeS n . stream
-- "take"     [~1] forall n xs . take n xs =
--   build (\c nil -> if 0 < n
--                    then foldr (takeFB c nil) (flipSeq nil) xs n
--                    else nil)
-- "unsafeTakeList"  [1] forall n xs . foldr (takeFB (:) []) (flipSeq []) xs n
--                                         = unsafeTake n xs
 #-}

{-# INLINE [0] flipSeq #-}
-- Just flip seq, but not inlined too early.
-- It's important to force the argument here, even though it's not used.
-- Otherwise, take n [] can't unbox n, leading to increased allocation in T7257.
flipSeq :: a -> b -> a
flipSeq x !_n = x

{-# INLINE [0] takeFB #-} -- See Note [Inline FB functions]
takeFB :: (a -> b -> b) -> b -> a -> (Int -> b) -> Int -> b
-- The \m accounts for the fact that takeFB is used in a higher-order
-- way by takeFoldr, so it's better to inline.  A good example is
--     take n (repeat x)
-- for which we get excellent code... but only if we inline takeFB
-- when given four arguments
takeFB c n x xs
  = \ m -> case m of
            1 -> x `c` n
            _ -> x `c` xs (m - 1)
#endif

-- | 'drop' @n xs@ returns the suffix of @xs@
-- after the first @n@ elements, or @[]@ if @n >= 'length' xs@.
--
-- It is an instance of the more general 'GHC.Internal.Data.List.genericDrop',
-- in which @n@ may be of any integral type.
--
-- ==== __Examples__
--
-- >>> drop 6 "Hello World!"
-- "World!"
--
-- >>> drop 3 [1,2,3,4,5]
-- [4,5]
--
-- >>> drop 3 [1,2]
-- []
--
-- >>> drop 3 []
-- []
--
-- >>> drop (-1) [1,2]
-- [1,2]
--
-- >>> drop 0 [1,2]
-- [1,2]
drop                   :: Int -> [a] -> [a]
#if defined(USE_REPORT_PRELUDE)
drop n xs     | n <= 0 =  xs
drop _ []              =  []
drop n (_:xs)          =  drop (n-1) xs
#else /* hack away */
{-# INLINE [1] drop #-}
drop n ls
  | n <= 0     = ls
  | otherwise  = unsafeDrop n ls
  where
    -- A version of drop that drops the whole list if given an argument
    -- less than 1
    unsafeDrop :: Int -> [a] -> [a]
    unsafeDrop !_ []     = []
    unsafeDrop 1  (_:xs) = xs
    unsafeDrop m  (_:xs) = unsafeDrop (m - 1) xs
{-# RULES
"drop/unstream" forall n s . drop n (unstream s) = unstream (dropS n s)
"drop/cheapUnstream" forall n s. drop n (cheapUnstream s) = cheapUnstream (dropS n s)
 #-}
#endif

-- | 'splitAt' @n xs@ returns a tuple where first element is @xs@ prefix of
-- length @n@ and second element is the remainder of the list:
--
-- 'splitAt' is an instance of the more general 'GHC.Internal.Data.List.genericSplitAt',
-- in which @n@ may be of any integral type.
--
-- ==== __Laziness__
--
-- It is equivalent to @('take' n xs, 'drop' n xs)@
-- unless @n@ is @_|_@:
-- @splitAt _|_ xs = _|_@, not @(_|_, _|_)@).
--
-- The first component of the tuple is produced lazily:
--
-- >>> fst (splitAt 0 undefined)
-- []
--
-- >>> take 1 (fst (splitAt 10 (1 : undefined)))
-- [1]
--
-- ==== __Examples__
--
-- >>> splitAt 6 "Hello World!"
-- ("Hello ","World!")
--
-- >>> splitAt 3 [1,2,3,4,5]
-- ([1,2,3],[4,5])
--
-- >>> splitAt 1 [1,2,3]
-- ([1],[2,3])
--
-- >>> splitAt 3 [1,2,3]
-- ([1,2,3],[])
--
-- >>> splitAt 4 [1,2,3]
-- ([1,2,3],[])
--
-- >>> splitAt 0 [1,2,3]
-- ([],[1,2,3])
--
-- >>> splitAt (-1) [1,2,3]
-- ([],[1,2,3])
splitAt                :: Int -> [a] -> ([a],[a])

#if defined(USE_REPORT_PRELUDE)
splitAt n xs           =  (take n xs, drop n xs)
#else
splitAt n ls
  | n <= 0 = ([], ls)
  | otherwise          = splitAt' n ls
    where
        splitAt' :: Int -> [a] -> ([a], [a])
        splitAt' _  []     = ([], [])
        splitAt' 1  (x:xs) = ([x], xs)
        splitAt' m  (x:xs) = (x:xs', xs'')
          where
            (xs', xs'') = splitAt' (m - 1) xs
{-# NOINLINE [0] splitAt #-}
{-# RULES
"splitAt/unstream" forall n s. splitAt n (unstream s) = splitAtS n s
"splitAt/cheapUnstream" forall n s. splitAt n (cheapUnstream s) = cheapSplitAtS n s
 #-}
#endif /* USE_REPORT_PRELUDE */

-- | 'span', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- first element is the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@ and second element is the remainder of the list:
--
-- 'span' @p xs@ is equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@, even if @p@ is @_|_@.
--
-- ==== __Laziness__
--
-- >>> span undefined []
-- ([],[])
-- >>> fst (span (const False) undefined)
-- *** Exception: Prelude.undefined
-- >>> fst (span (const False) (undefined : undefined))
-- []
-- >>> take 1 (fst (span (const True) (1 : undefined)))
-- [1]
--
-- 'span' produces the first component of the tuple lazily:
--
-- >>> take 10 (fst (span (const True) [1..]))
-- [1,2,3,4,5,6,7,8,9,10]
--
-- ==== __Examples__
--
-- >>> span (< 3) [1,2,3,4,1,2,3,4]
-- ([1,2],[3,4,1,2,3,4])
--
-- >>> span (< 9) [1,2,3]
-- ([1,2,3],[])
--
-- >>> span (< 0) [1,2,3]
-- ([],[1,2,3])
span                    :: (a -> Bool) -> [a] -> ([a],[a])
span _ xs@[]            =  (xs, xs)
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)

-- | 'break', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- first element is longest prefix (possibly empty) of @xs@ of elements that
-- /do not satisfy/ @p@ and second element is the remainder of the list:
--
-- 'break' @p@ is equivalent to @'span' ('not' . p)@
-- and consequently to @('takeWhile' ('not' . p) xs, 'dropWhile' ('not' . p) xs)@,
-- even if @p@ is @_|_@.
--
-- ==== __Laziness__
--
-- >>> break undefined []
-- ([],[])
--
-- >>> fst (break (const True) undefined)
-- *** Exception: Prelude.undefined
--
-- >>> fst (break (const True) (undefined : undefined))
-- []
--
-- >>> take 1 (fst (break (const False) (1 : undefined)))
-- [1]
--
-- 'break' produces the first component of the tuple lazily:
--
-- >>> take 10 (fst (break (const False) [1..]))
-- [1,2,3,4,5,6,7,8,9,10]
--
-- ==== __Examples__
--
-- >>> break (> 3) [1,2,3,4,1,2,3,4]
-- ([1,2,3],[4,1,2,3,4])
--
-- >>> break (< 9) [1,2,3]
-- ([],[1,2,3])
--
-- >>> break (> 9) [1,2,3]
-- ([1,2,3],[])
break                   :: (a -> Bool) -> [a] -> ([a],[a])
#if defined(USE_REPORT_PRELUDE)
break p                 =  span (not . p)
#else
-- HBC version (stolen)
break _ xs@[]           =  (xs, xs)
break p xs@(x:xs')
           | p x        =  ([],xs)
           | otherwise  =  let (ys,zs) = break p xs' in (x:ys,zs)
#endif

-- | \(\mathcal{O}(n)\). 'reverse' @xs@ returns the elements of @xs@ in reverse order.
-- @xs@ must be finite.
--
-- ==== __Laziness__
--
-- 'reverse' is lazy in its elements.
--
-- >>> head (reverse [undefined, 1])
-- 1
--
-- >>> reverse (1 : 2 : undefined)
-- *** Exception: Prelude.undefined
--
-- ==== __Examples__
--
-- >>> reverse []
-- []
--
-- >>> reverse [42]
-- [42]
--
-- >>> reverse [2,5,7]
-- [7,5,2]
--
-- >>> reverse [1..]
-- * Hangs forever *
reverse                 :: [a] -> [a]
#if defined(USE_REPORT_PRELUDE)
reverse                 =  foldl (flip (:)) []
#else
reverse l =  rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)
#endif

-- | 'and' returns the conjunction of a Boolean list. For the result to be
-- 'True', the list must be finite; 'False', however, results from a 'False'
-- value at a finite index of a finite or infinite list.
--
-- ==== __Examples__
--
-- >>> and []
-- True
--
-- >>> and [True]
-- True
--
-- >>> and [False]
-- False
--
-- >>> and [True, True, False]
-- False
--
-- >>> and (False : repeat True) -- Infinite list [False,True,True,True,True,True,True...
-- False
--
-- >>> and (repeat True)
-- * Hangs forever *
and                     :: [Bool] -> Bool
#if defined(USE_REPORT_PRELUDE)
and                     =  foldr (&&) True
#else
and []          =  True
and (x:xs)      =  x && and xs
{-# NOINLINE [1] and #-}

{-# RULES
"and" and = andS . stream
-- "and/build"     forall (g::forall b.(Bool->b->b)->b->b) .
--                 and (build g) = g (&&) True
 #-}
#endif

-- | 'or' returns the disjunction of a Boolean list. For the result to be
-- 'False', the list must be finite; 'True', however, results from a 'True'
-- value at a finite index of a finite or infinite list.
--
-- ==== __Examples__
--
-- >>> or []
-- False
--
-- >>> or [True]
-- True
--
-- >>> or [False]
-- False
--
-- >>> or [True, True, False]
-- True
--
-- >>> or (True : repeat False) -- Infinite list [True,False,False,False,False,False,False...
-- True
--
-- >>> or (repeat False)
-- * Hangs forever *
or                      :: [Bool] -> Bool
#if defined(USE_REPORT_PRELUDE)
or                      =  foldr (||) False
#else
or []           =  False
or (x:xs)       =  x || or xs
{-# NOINLINE [1] or #-}

{-# RULES
"or" or = orS . stream
-- "or/build"      forall (g::forall b.(Bool->b->b)->b->b) .
--                 or (build g) = g (||) False
 #-}
#endif

-- | Applied to a predicate and a list, 'any' determines if any element
-- of the list satisfies the predicate. For the result to be
-- 'False', the list must be finite; 'True', however, results from a 'True'
-- value for the predicate applied to an element at a finite index of a finite
-- or infinite list.
--
-- ==== __Examples__
--
-- >>> any (> 3) []
-- False
--
-- >>> any (> 3) [1,2]
-- False
--
-- >>> any (> 3) [1,2,3,4,5]
-- True
--
-- >>> any (> 3) [1..]
-- True
--
-- >>> any (> 3) [0, -1..]
-- * Hangs forever *
any                     :: (a -> Bool) -> [a] -> Bool
#if defined(USE_REPORT_PRELUDE)
any p                   =  or . map p
#else
any _ []        = False
any p (x:xs)    = p x || any p xs

{-# NOINLINE [1] any #-}

{-# RULES
"any" forall p . any p = anyS p . stream
-- "any/build"     forall p (g::forall b.(a->b->b)->b->b) .
--                 any p (build g) = g ((||) . p) False
 #-}
#endif

-- | Applied to a predicate and a list, 'all' determines if all elements
-- of the list satisfy the predicate. For the result to be
-- 'True', the list must be finite; 'False', however, results from a 'False'
-- value for the predicate applied to an element at a finite index of a finite
-- or infinite list.
--
-- ==== __Examples__
--
-- >>> all (> 3) []
-- True
--
-- >>> all (> 3) [1,2]
-- False
--
-- >>> all (> 3) [1,2,3,4,5]
-- False
--
-- >>> all (> 3) [1..]
-- False
--
-- >>> all (> 3) [4..]
-- * Hangs forever *
all                     :: (a -> Bool) -> [a] -> Bool
#if defined(USE_REPORT_PRELUDE)
all p                   =  and . map p
#else
all _ []        =  True
all p (x:xs)    =  p x && all p xs

{-# NOINLINE [1] all #-}

{-# RULES
"all" forall p . all p = allS p . stream
-- "all/build"     forall p (g::forall b.(a->b->b)->b->b) .
--                 all p (build g) = g ((&&) . p) True
 #-}
#endif

-- | 'elem' is the list membership predicate, usually written in infix form,
-- e.g., @x \`elem\` xs@.  For the result to be
-- 'False', the list must be finite; 'True', however, results from an element
-- equal to @x@ found at a finite index of a finite or infinite list.
--
-- ==== __Examples__
--
-- >>> 3 `elem` []
-- False
--
-- >>> 3 `elem` [1,2]
-- False
--
-- >>> 3 `elem` [1,2,3,4,5]
-- True
--
-- >>> 3 `elem` [1..]
-- True
--
-- >>> 3 `elem` [4..]
-- * Hangs forever *
elem                    :: (Eq a) => a -> [a] -> Bool
#if defined(USE_REPORT_PRELUDE)
elem x                  =  any (== x)
#else
elem _ []       = False
elem x (y:ys)   = x==y || elem x ys
{-# NOINLINE [1] elem #-}
{-# RULES
"elem" forall x. elem x = elemS x . stream
-- "elem/build"    forall x (g :: forall b . (a -> b -> b) -> b -> b)
--    . elem x (build g) = g (\ y r -> (x == y) || r) False
 #-}
#endif

-- | 'notElem' is the negation of 'elem'.
--
-- ==== __Examples__
--
-- >>> 3 `notElem` []
-- True
--
-- >>> 3 `notElem` [1,2]
-- True
--
-- >>> 3 `notElem` [1,2,3,4,5]
-- False
--
-- >>> 3 `notElem` [1..]
-- False
--
-- >>> 3 `notElem` [4..]
-- * Hangs forever *
notElem                 :: (Eq a) => a -> [a] -> Bool
#if defined(USE_REPORT_PRELUDE)
notElem x               =  all (/= x)
#else
notElem _ []    =  True
notElem x (y:ys)=  x /= y && notElem x ys
{-# NOINLINE [1] notElem #-}
{-# RULES
"notElem" forall x. notElem x = notElemS x . stream
-- "notElem/build" forall x (g :: forall b . (a -> b -> b) -> b -> b)
--    . notElem x (build g) = g (\ y r -> (x /= y) && r) True
 #-}
#endif

-- | \(\mathcal{O}(n)\). 'lookup' @key assocs@ looks up a key in an association
-- list.
-- For the result to be 'Nothing', the list must be finite.
--
-- ==== __Examples__
--
-- >>> lookup 2 []
-- Nothing
--
-- >>> lookup 2 [(1, "first")]
-- Nothing
--
-- >>> lookup 2 [(1, "first"), (2, "second"), (3, "third")]
-- Just "second"
lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup _key []          =  Nothing
lookup  key ((x,y):xys)
    | key == x           =  Just y
    | otherwise         =  lookup key xys
{-# NOINLINE [1] lookup #-} -- see Note [Fusion for lookup]
{-# RULES
"lookup" forall x. lookup x = lookupS x . stream
-- "lookup/build" forall x (g :: forall b. ((k, a) -> b -> b) -> b -> b).
--   lookup x (build g) = g (\(k, v) r -> if x == k then Just v else r) Nothing
#-}


-- | Concatenate a list of lists.
--
-- ==== __Examples__
--
-- >>> concat [[1,2,3], [4,5], [6], []]
-- [1,2,3,4,5,6]
--
-- >>> concat []
-- []
--
-- >>> concat [[42]]
-- [42]
concat :: [[a]] -> [a]
concat = foldr (++) []

{-# NOINLINE [1] concat #-}

{-# RULES
"concat" concat = unstream . concatS . stream
--   "concat" forall xs. concat xs =
--      build (\c n -> foldr (\x y -> foldr c y x) n xs)
-- We don't bother to turn non-fusible applications of concat back into concat
 #-}

-- | List index (subscript) operator, starting from 0.
-- It is an instance of the more general 'GHC.Internal.Data.List.genericIndex',
-- which takes an index of any integral type.
--
-- WARNING: This function is partial, and should only be used if you are
-- sure that the indexing will not fail. Otherwise, use 'Data.List.!?'.
--
-- WARNING: This function takes linear time in the index.
--
-- ==== __Examples__
--
-- >>> ['a', 'b', 'c'] !! 0
-- 'a'
--
-- >>> ['a', 'b', 'c'] !! 2
-- 'c'
--
-- >>> ['a', 'b', 'c'] !! 3
-- *** Exception: Prelude.!!: index too large
--
-- >>> ['a', 'b', 'c'] !! (-1)
-- *** Exception: Prelude.!!: negative index
#if defined(USE_REPORT_PRELUDE)
(!!)                    :: [a] -> Int -> a
xs     !! n | n < 0 =  errorWithoutStackTrace "Prelude.!!: negative index"
[]     !! _         =  errorWithoutStackTrace "Prelude.!!: index too large"
(x:_)  !! 0         =  x
(_:xs) !! n         =  xs !! (n-1)
-- Prelude version is without HasCallStack to avoid building linear one
#else
(!!)                    :: HasCallStack => [a] -> Int -> a

-- We don't really want the errors to inline with (!!).
-- We may want to fuss around a bit with NOINLINE, and
-- if so we should be careful not to trip up known-bottom
-- optimizations.
tooLarge :: HasCallStack => Int -> a
tooLarge _ = error (prel_list_str ++ "!!: index too large")

negIndex :: HasCallStack => a
negIndex = error $ prel_list_str ++ "!!: negative index"

{-# INLINABLE (!!) #-}
xs !! n
  | n < 0     = negIndex
  | otherwise = unsafeIndexHelper xs n

unsafeIndexHelper :: [a] -> Int -> a
unsafeIndexHelper [] i = tooLarge i
unsafeIndexHelper (x:xs) 0 = x
unsafeIndexHelper (x:xs) n = unsafeIndexHelper xs (n - 1)
{-# NOINLINE [1] unsafeIndexHelper #-}

{-# RULES
"unsafeIndexHelper" forall xs. unsafeIndexHelper xs = unsafeIndexHelperS (stream xs)
 #-}
#endif

-- | List index (subscript) operator, starting from 0. Returns 'Nothing'
-- if the index is out of bounds
--
-- This is the total variant of the partial '!!' operator.
--
-- WARNING: This function takes linear time in the index.
--
-- ==== __Examples__
--
-- >>> ['a', 'b', 'c'] !? 0
-- Just 'a'
--
-- >>> ['a', 'b', 'c'] !? 2
-- Just 'c'
--
-- >>> ['a', 'b', 'c'] !? 3
-- Nothing
--
-- >>> ['a', 'b', 'c'] !? (-1)
-- Nothing
(!?) :: [a] -> Int -> Maybe a

{-# INLINABLE (!?) #-}
xs !? n
  | n < 0     = Nothing
  | otherwise = indexHelper xs n

indexHelper :: [a] -> Int -> Maybe a
indexHelper [] _ = Nothing
indexHelper (x:_) 0 = Just x
indexHelper (_:xs) n = indexHelper xs (n - 1)
{-# NOINLINE [1] indexHelper #-}
{-# RULES
"indexHelper" forall xs n . indexHelper xs n = indexHelperS (stream xs) n
 #-}

--------------------------------------------------------------
-- The zip family
--------------------------------------------------------------

foldr2 :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
foldr2 k z = go
  where
        go []    _ys     = z
        go _xs   []      = z
        go (x:xs) (y:ys) = k x y (go xs ys)
{-# INLINE [0] foldr2 #-}  -- See Note [Fusion for foldrN]

foldr2_left :: (a -> b -> c -> d) -> d -> a -> ([b] -> c) -> [b] -> d
foldr2_left _k  z _x _r []     = z
foldr2_left  k _z  x  r (y:ys) = k x y (r ys)

-- foldr2 k z xs ys = foldr (foldr2_left k z)  (\_ -> z) xs ys
{-# RULES   -- See Note [Fusion for foldrN]
"foldr2/left"   forall k z ys (g::forall b.(a->b->b)->b->b) .
                  foldr2 k z (build g) ys = g (foldr2_left  k z) (\_ -> z) ys
 #-}

foldr3 :: (a -> b -> c -> d -> d) -> d -> [a] -> [b] -> [c] -> d
foldr3 k z = go
  where
    go  []    _      _      = z
    go  _     []     _      = z
    go  _     _      []     = z
    go (a:as) (b:bs) (c:cs) = k a b c (go as bs cs)
{-# INLINE [0] foldr3 #-}  -- See Note [Fusion for foldrN]


foldr3_left :: (a -> b -> c -> d -> e) -> e -> a ->
               ([b] -> [c] -> d) -> [b] -> [c] -> e
foldr3_left k _z a r (b:bs) (c:cs) = k a b c (r bs cs)
foldr3_left _  z _ _  _      _     = z

-- foldr3 k n xs ys zs = foldr (foldr3_left k n) (\_ _ -> n) xs ys zs
{-# RULES   -- See Note [Fusion for foldrN]
"foldr3/left"   forall k z (g::forall b.(a->b->b)->b->b).
                  foldr3 k z (build g) = g (foldr3_left k z) (\_ _ -> z)
 #-}

{-
Note [Fusion for foldrN]
~~~~~~~~~~~~~~~~~~~~~~~~
We arrange that foldr2, foldr3, etc is a good consumer for its first
(left) list argument. Here's how. See below for the second, third
etc list arguments

* The rule "foldr2/left" (active only before phase 1) does this:
     foldr2 k z (build g) ys = g (foldr2_left  k z) (\_ -> z) ys
  thereby fusing away the 'build' on the left argument

* To ensure this rule has a chance to fire, foldr2 has a NOINLINE[1] pragma

There used to be a "foldr2/right" rule, allowing foldr2 to fuse with a build
form on the right. However, this causes trouble if the right list ends in
a bottom that is only avoided by the left list ending at that spot. That is,
foldr2 f z [a,b,c] (d:e:f:_|_), where the right list is produced by a build
form, would cause the foldr2/right rule to introduce bottom. Example:
  zip [1,2,3,4] (unfoldr (\s -> if s > 4 then undefined else Just (s,s+1)) 1)
should produce
  [(1,1),(2,2),(3,3),(4,4)]
but with the foldr2/right rule it would instead produce
  (1,1):(2,2):(3,3):(4,4):_|_

Note [Fusion for zipN/zipWithN]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We arrange that zip, zip3, etc, and zipWith, zipWit3 etc, are all
good consumers for their first (left) argument, and good producers.
Here's how.  See Note [Fusion for foldrN] for why it can't fuse its
second (right) list argument.

NB: Zips for larger tuples are in the List module.

* Rule "zip" (active only before phase 1) rewrites
    zip xs ys = build (\c n -> foldr2 (zipFB c) n xs ys)
  See also Note [Inline FB functions]

  Ditto rule "zipWith".

* To give this rule a chance to fire, we give zip a NOLINLINE[1]
  pragma (although since zip is recursive it might not need it)

* Now the rules for foldr2 (see Note [Fusion for foldrN]) may fire,
  or rules that fuse the build-produced output of zip.

* If none of these fire, rule "zipList" (active only in phase 1)
  rewrites the foldr2 call back to zip
     foldr2 (zipFB (:)) []   = zip
  This rule will only fire when build has inlined, which also
  happens in phase 1.

  Ditto rule "zipWithList".

Note [Fusion for lookup]
~~~~~~~~~~~~~~~~~~~~~~~~
Implementing lookup with foldr has the potential to cause code duplication
if fusion doesn't occur, so we use RULES instead so that lookup can participate
in list fusion.
The NONINLINE pragma gives the RULE a chance to fire.
It's recursive, so won't inline anyway, but saying so is more explicit.
See the discussion in https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10715/
-}

----------------------------------------------
-- | \(\mathcal{O}(\min(m,n))\). 'zip' takes two lists and returns a list of
-- corresponding pairs.
--
-- 'zip' is right-lazy:
--
-- >>> zip [] undefined
-- []
-- >>> zip undefined []
-- *** Exception: Prelude.undefined
-- ...
--
-- 'zip' is capable of list fusion, but it is restricted to its
-- first list argument and its resulting list.
--
-- ==== __Examples__
--
-- >>> zip [1, 2, 3] ['a', 'b', 'c']
-- [(1,'a'),(2,'b'),(3,'c')]
--
-- If one input list is shorter than the other, excess elements of the longer
-- list are discarded, even if one of the lists is infinite:
--
-- >>> zip [1] ['a', 'b']
-- [(1,'a')]
--
-- >>> zip [1, 2] ['a']
-- [(1,'a')]
--
-- >>> zip [] [1..]
-- []
--
-- >>> zip [1..] []
-- []
{-# NOINLINE [1] zip #-}  -- See Note [Fusion for zipN/zipWithN]
zip :: [a] -> [b] -> [(a,b)]
zip []     _bs    = []
zip _as    []     = []
zip (a:as) (b:bs) = (a,b) : zip as bs

{-# INLINE [0] zipFB #-} -- See Note [Inline FB functions]
zipFB :: ((a, b) -> c -> d) -> a -> b -> c -> d
zipFB c = \x y r -> (x,y) `c` r

{-# RULES  -- See Note [Fusion for zipN/zipWithN]
"zip" forall xs ys. zip xs ys = unstream (zipS (stream xs) (stream ys))
-- "zip"      [~1] forall xs ys. zip xs ys = build (\c n -> foldr2 (zipFB c) n xs ys)
-- "zipList"  [1]  foldr2 (zipFB (:)) []   = zip
 #-}

----------------------------------------------
-- | 'zip3' takes three lists and returns a list of triples, analogous to
-- 'zip'.
-- It is capable of list fusion, but it is restricted to its
-- first list argument and its resulting list.
{-# NOINLINE [1] zip3 #-}
zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
-- Specification
-- zip3 =  zipWith3 (,,)
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
zip3 _      _      _      = []

{-# INLINE [0] zip3FB #-} -- See Note [Inline FB functions]
zip3FB :: ((a,b,c) -> xs -> xs') -> a -> b -> c -> xs -> xs'
zip3FB cons = \a b c r -> (a,b,c) `cons` r

{-# RULES    -- See Note [Fusion for zipN/zipWithN]
"zip3" forall xs ys zs . zip3 xs ys zs = unstream (zip3S (stream xs) (stream ys) (stream zs))
-- "zip3"       [~1] forall as bs cs. zip3 as bs cs = build (\c n -> foldr3 (zip3FB c) n as bs cs)
-- "zip3List"   [1]          foldr3 (zip3FB (:)) [] = zip3
 #-}

-- The zipWith family generalises the zip family by zipping with the
-- function given as the first argument, instead of a tupling function.

----------------------------------------------
-- | \(\mathcal{O}(\min(m,n))\). 'zipWith' generalises 'zip' by zipping with the
-- function given as the first argument, instead of a tupling function.
--
-- > zipWith (,) xs ys == zip xs ys
-- > zipWith f [x1,x2,x3..] [y1,y2,y3..] == [f x1 y1, f x2 y2, f x3 y3..]
--
--
-- 'zipWith' is right-lazy:
--
-- >>> let f = undefined
-- >>> zipWith f [] undefined
-- []
--
-- 'zipWith' is capable of list fusion, but it is restricted to its
-- first list argument and its resulting list.
--
-- ==== __Examples__
--
-- @'zipWith' '(+)'@ can be applied to two lists to produce the list of
-- corresponding sums:
--
-- >>> zipWith (+) [1, 2, 3] [4, 5, 6]
-- [5,7,9]
--
-- >>> zipWith (++) ["hello ", "foo"] ["world!", "bar"]
-- ["hello world!","foobar"]
{-# NOINLINE [1] zipWith #-}  -- See Note [Fusion for zipN/zipWithN]
zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith f = go
  where
    go [] _ = []
    go _ [] = []
    go (x:xs) (y:ys) = f x y : go xs ys

-- zipWithFB must have arity 2 since it gets two arguments in the "zipWith"
-- rule; it might not get inlined otherwise
{-# INLINE [0] zipWithFB #-} -- See Note [Inline FB functions]
zipWithFB :: (a -> b -> c) -> (d -> e -> a) -> d -> e -> b -> c
zipWithFB c f = \x y r -> (x `f` y) `c` r

{-# RULES       -- See Note [Fusion for zipN/zipWithN]
"zipWith" forall f xs ys. zipWith f xs ys = unstream (zipWithS f (stream xs) (stream ys))
-- "zipWith"       [~1] forall f xs ys.    zipWith f xs ys = build (\c n -> foldr2 (zipWithFB c f) n xs ys)
-- "zipWithList"   [1]  forall f.  foldr2 (zipWithFB (:) f) [] = zipWith f
  #-}

-- | \(\mathcal{O}(\min(l,m,n))\). The 'zipWith3' function takes a function which combines three
-- elements, as well as three lists and returns a list of the function applied
-- to corresponding elements, analogous to 'zipWith'.
-- It is capable of list fusion, but it is restricted to its
-- first list argument and its resulting list.
--
-- > zipWith3 (,,) xs ys zs == zip3 xs ys zs
-- > zipWith3 f [x1,x2,x3..] [y1,y2,y3..] [z1,z2,z3..] == [f x1 y1 z1, f x2 y2 z2, f x3 y3 z3..]
--
-- ==== __Examples__
--
-- >>> zipWith3 (\x y z -> [x, y, z]) "123" "abc" "xyz"
-- ["1ax","2by","3cz"]
--
-- >>> zipWith3 (\x y z -> (x * y) + z) [1, 2, 3] [4, 5, 6] [7, 8, 9]
-- [11,18,27]
{-# NOINLINE [1] zipWith3 #-}
zipWith3                :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z = go
  where
    go (a:as) (b:bs) (c:cs) = z a b c : go as bs cs
    go _ _ _                = []

{-# INLINE [0] zipWith3FB #-} -- See Note [Inline FB functions]
zipWith3FB :: (d -> xs -> xs') -> (a -> b -> c -> d) -> a -> b -> c -> xs -> xs'
zipWith3FB cons func = \a b c r -> (func a b c) `cons` r

{-# RULES
"zipWith3" forall f xs ys zs . zipWith3 f xs ys zs = unstream (zipWith3S f (stream xs) (stream ys) (stream zs))
-- "zipWith3"      [~1] forall f as bs cs.   zipWith3 f as bs cs = build (\c n -> foldr3 (zipWith3FB c f) n as bs cs)
-- "zipWith3List"  [1]  forall f.   foldr3 (zipWith3FB (:) f) [] = zipWith3 f
 #-}

-- | 'unzip' transforms a list of pairs into a list of first components
-- and a list of second components.
--
-- ==== __Examples__
--
-- >>> unzip []
-- ([],[])
--
-- >>> unzip [(1, 'a'), (2, 'b')]
-- ([1,2],"ab")
unzip    :: [(a,b)] -> ([a],[b])
{-# INLINE unzip #-}
-- Inline so that fusion `foldr` has an opportunity to fire.
-- See Note [Inline @unzipN@ functions] in GHC/OldList.hs.
unzip    =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])

-- | The 'unzip3' function takes a list of triples and returns three
-- lists of the respective components, analogous to 'unzip'.
--
-- ==== __Examples__
--
-- >>> unzip3 []
-- ([],[],[])
--
-- >>> unzip3 [(1, 'a', True), (2, 'b', False)]
-- ([1,2],"ab",[True,False])
unzip3   :: [(a,b,c)] -> ([a],[b],[c])
{-# INLINE unzip3 #-}
-- Inline so that fusion `foldr` has an opportunity to fire.
-- See Note [Inline @unzipN@ functions] in GHC/OldList.hs.
unzip3   =  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
                  ([],[],[])

--------------------------------------------------------------
-- Error code
--------------------------------------------------------------

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:

errorEmptyList :: HasCallStack => String -> a
errorEmptyList fun =
  error (prel_list_str ++ fun ++ ": empty list")

prel_list_str :: String
prel_list_str = "Prelude."
