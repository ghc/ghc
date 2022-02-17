module T21392 (f) where

import Data.List (foldl', insertBy)
import Data.Ord

newtype Unique = U { unU :: Int }
class Uniquable u where getKey :: u -> Unique
instance Uniquable Int where getKey = U
data UMap a = UMap { unS :: ![(Unique,a)], unI :: !Int }

addOne :: Uniquable u => UMap a -> u -> a -> UMap a
addOne (UMap set n) x v = UMap (insertBy (comparing (unU . fst)) (getKey x,v) set) (n+1)

newtype USet u = USet (UMap u)

insertOne :: Uniquable u => USet u -> u -> USet u
insertOne (USet s) x = USet (addOne s x x)

insertMany :: Uniquable u => USet u -> [u] -> USet u
insertMany s vs = foldl' insertOne s (reverse (reverse vs))

seq' = seq
{-# NOINLINE seq' #-}

blah s@(USet m) = unS m `seq'` s
{-# NOINLINE blah #-}

end (USet m) = unS m
{-# NOINLINE end #-}

f :: USet Int -> [Int] -> [(Unique,Int)]
f !xs ys
  | length ys == 13 = end $ blah t
  | length ys == 23 = reverse $ end $ blah t
  | otherwise       = []
  where
    t = insertMany xs (reverse $ reverse $ reverse $ reverse ys)
