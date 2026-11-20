{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeFamilies #-}
module Filter (Hidden, familyHide, FilteredWith, SameLength, Product(..), ProductWith, ProductSame,
               makeProduct, filterVec, partitionVec, toList, fromList) where
import Vector

type Hidden a = exists (m :: Nat). Vec m a

type FilteredWith a r = exists (m :: Nat). (Vec m a, Vec m a -> r)
type SameLength a = exists (m :: Nat). (Vec m a, Vec m a)

data Product a b = Product a b

type ProductWith a r = exists (m :: Nat). Product (Vec m a) (Vec m a -> r)
type ProductSame a = exists (m :: Nat). Product (Vec m a) (Vec m a)

-- | Construct the fields of the product in declaration order.
makeProduct :: a -> b -> Product a b
makeProduct a b = Product a b

hide :: Vec n a -> Hidden a
hide xs = xs

type family FamilyHidden a where
    FamilyHidden a = Maybe (Hidden a)

familyHide :: forall a n. Vec n a -> FamilyHidden a
familyHide xs = (Just (hide xs) :: Maybe (Hidden a))

filterVec :: (a -> Bool) -> Vec n a -> Hidden a
filterVec _ Nil = Nil
filterVec p (Cons x xs)
    | p x = Cons x (filterVec p xs)
    | otherwise = filterVec p xs

partitionVec :: (a -> Bool) -> Vec n a -> (Hidden a, Hidden a)
partitionVec _ Nil = (Nil, Nil)
partitionVec p (Cons x xs) =
    let (yes, no) = partitionVec p xs
    in if p x then (Cons x yes, no) else (yes, Cons x no)

toList :: Hidden a -> [a]
toList xs = go xs
  where
    go :: Vec n a -> [a]
    go Nil = []
    go (Cons x rest) = x : go rest

-- | Build a vector that hides the length of the input list.
fromList :: [a] -> Hidden a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)
