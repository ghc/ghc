module ShouldSucceed where

data T a = D (B a) | C
data B b = X | Y b

instance (Eq a) => Eq (T a) where
 (D x) == (D y) = x == y
 C == C = True
 a == b = False

 a /= b = not (a == b)

instance (Eq b) => Eq (B b) where
 X == X = True
 (Y a) == (Y b) = a == b
 a == b = False

 a /= b = not (a == b)
