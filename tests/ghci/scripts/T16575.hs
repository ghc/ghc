module Ghost where

data X = A | B
    deriving (Show)

instance Eq X where
    A == A = True
    B == B = True
    _ == _ = False
