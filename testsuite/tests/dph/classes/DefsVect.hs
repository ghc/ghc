{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

module DefsVect where

import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Bool
import Data.Array.Parallel.Prelude.Int  (Int, sumP)


{-# VECTORISE class Eq #-}
{-# VECTORISE SCALAR instance Eq Bool #-}
-- {-# VECTORISE SCALAR instance Eq Char #-}
{-# VECTORISE SCALAR instance Eq Int #-}
{-# VECTORISE SCALAR instance Eq Word8 #-}
-- {-# VECTORISE SCALAR instance Eq Float #-}
{-# VECTORISE SCALAR instance Eq Double #-}
{-# VECTORISE SCALAR instance Eq Ordering #-}
{-# VECTORISE class Ord #-}
{-# VECTORISE SCALAR instance Ord Bool #-}
-- {-# VECTORISE SCALAR instance Ord Char #-}
{-# VECTORISE SCALAR instance Ord Int #-}
{-# VECTORISE SCALAR instance Ord Word8 #-}
-- {-# VECTORISE SCALAR instance Ord Float #-}
{-# VECTORISE SCALAR instance Ord Double #-}
{-# VECTORISE SCALAR instance Ord Ordering #-}


data MyBool = MyTrue | MyFalse

class Eq a => Cmp a where
  cmp :: a -> a -> Bool

isFive :: Int -> Bool
isFive x = x == 5

isEq :: Eq a => a -> Bool
isEq x = x == x

fiveEq :: Int -> Bool
fiveEq x = isFive x && isEq x

cmpArrs :: PArray Int -> PArray Int -> Bool
{-# NOINLINE cmpArrs #-}
cmpArrs v w = cmpArrs' (fromPArrayP v) (fromPArrayP w)

cmpArrs' :: [:Int:] -> [:Int:] -> Bool
cmpArrs' xs ys = andP [:x == y | x <- xs | y <- ys:]

isFives :: PArray Int -> Bool
{-# NOINLINE isFives #-}
isFives xs = isFives' (fromPArrayP xs)

isFives' :: [:Int:] -> Bool
isFives' xs = andP (mapP isFive xs)

isEqs :: PArray Int -> Bool
{-# NOINLINE isEqs #-}
isEqs xs = isEqs' (fromPArrayP xs)

isEqs' :: [:Int:] -> Bool
isEqs' xs = andP (mapP isEq xs)
