module T16114 where

data T a
instance Eq a => Eq a => Eq (T a) where (==) = undefined
