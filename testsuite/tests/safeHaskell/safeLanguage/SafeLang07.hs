{-# LANGUAGE Safe #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Here we stop it succeeding (SAFE)

-- | We use newtype to create an isomorphic type to Int
-- with a reversed Ord dictionary. We now use the MinList
-- API of Y1 to create a new MinList. Then we use newtype
-- deriving to convert the newtype MinList to an Int
-- MinList. This final result breaks the invariants of
-- MinList which shouldn't be possible with the exposed
-- API of Y1.
module Main where

import SafeLang07_A

class IntIso t where
    intIso :: c t -> c Int

instance IntIso Int where
    intIso = id

newtype Down a = Down a deriving (Eq, Show, IntIso)

instance Ord a => Ord (Down a) where
    compare (Down a) (Down b) = compare b a

forceInt :: MinList Int -> MinList Int
forceInt = id

a1, a2 :: MinList Int
a1 = foldl insertMinList (newMinList $ head nums) (tail nums)
a2 = forceInt $ intIso $ foldl (\x y -> insertMinList x $ Down y) (newMinList $ Down $ head nums) (tail nums)

nums :: [Int]
nums = [1,4,0,1,-5,2,3,5,-1,2,0,0,-4,-3,9]

main = do
    printIntMinList a1
    printIntMinList a2

