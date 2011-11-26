{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

module DefsVect where

import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Bool
import Data.Array.Parallel.Prelude.Int  (Int)


{-# VECTORISE class Eq #-}
{-# VECTORISE SCALAR instance Eq Int #-}

-- {-# VECTORISE class Ord #-}
-- {-# VECTORISE SCALAR instance Ord Int #-}

-- {-# VECTORISE type Ordering #-}

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