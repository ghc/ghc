{-# OPTIONS_GHC -fvectorise #-}

module DefsVect where

import Data.Array.Parallel.Prelude.Bool
import Data.Array.Parallel.Prelude.Int  (Int)


{-# VECTORISE class Eq #-}
{-# VECTORISE SCALAR instance Eq Int #-}

class Eq a => Cmp a where
  cmp :: a -> a -> Bool

isFive :: Int -> Bool
isFive x = x == 5

isEq :: Eq a => a -> Bool
isEq x = x == x

fiveEq :: Int -> Bool
fiveEq x = isFive x && isEq x