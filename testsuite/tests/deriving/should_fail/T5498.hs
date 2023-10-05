{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | We use newtype to create an isomorphic type to Int
-- with a reversed Ord dictionary. We now use the MinList
-- API of MinList to create a new MinList. Then we use newtype
-- deriving to convert the newtype MinList to an Int
-- MinList. This final result breaks the invariants of
-- MinList which shouldn't be possible with the exposed
-- API of MinList.
module T5498 where

data MinList a = MinList a [a]

newMinList :: Ord a => a -> MinList a
newMinList n = MinList n []

insertMinList :: Ord a => MinList a -> a -> MinList a
insertMinList s@(MinList m xs) n | n > m     = MinList m (n:xs)
                                 | otherwise = s

printIntMinList :: MinList Int -> IO ()
printIntMinList (MinList min xs)
  = putStrLn $ "MinList Int :: MinList " ++ show min ++ " " ++ show xs

class IntIso t where
    intIso :: c t -> c Int

instance IntIso Int where
    intIso = id

newtype Down a = Down a deriving (Eq, IntIso)

instance Ord a => Ord (Down a) where
    compare (Down a) (Down b) = compare b a

fine :: MinList (Down Int)
fine = foldl (\x y -> insertMinList x $ Down y)
             (newMinList $ Down 0) [-1,-2,-3,-4,1,2,3,4]

bad :: MinList Int
bad = intIso fine

