{-# Language TypeFamilies #-}
module B where

newtype IntList = IntList [Int]

class ListLike a where
  type Elem a
  listOf :: (Elem a -> b) -> a -> [b]

instance ListLike IntList where
  type Elem IntList = Int
  listOf f (IntList xs) = map f xs

increment :: IntList -> [Int]
increment = listOf (+1)

newIntList :: [Int] -> IntList
newIntList = IntList
