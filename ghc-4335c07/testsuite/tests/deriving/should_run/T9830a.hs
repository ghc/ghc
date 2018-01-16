module T9830a where

infixr 6 :?:
data ADT a b = a :?: b deriving (Eq, Ord, Read)
