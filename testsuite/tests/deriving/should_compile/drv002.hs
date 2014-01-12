module ShouldSucceed where

data Z a b
  = C1 (T a)
  | C2 (Z [a] [b])
  deriving (Show, Read)

data T a
  = E1

instance Eq a => Show (T a) where
    showsPrec = error "show"
instance Eq a => Read (T a) where
    readsPrec = error "read"
