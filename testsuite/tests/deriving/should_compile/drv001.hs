-- !!! canonical weird example for "deriving"
module ShouldSucceed where

data X a b
  = C1 (T a)
  | C2 (Y b)
  | C3 (X b a)
  deriving (Read, Show)

data Y b
  = D1
  | D2 (X Int b)
  deriving (Read, Show)

data T a
  = E1

instance Eq a => Show (T a) where
    showsPrec = error "show"
instance Eq a => Read (T a) where
    readsPrec = error "read"
