--!!! Repeated type variable "a" in instance predicate
module M where
data T a b = MkT a b
instance Eq a => Eq (T a a)
