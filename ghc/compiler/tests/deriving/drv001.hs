--!!! canonical weird example for "deriving"

data X a b
  = C1 (T a)
  | C2 (Y b)
  | C3 (X b a)
  deriving Text

data Y b
  = D1
  | D2 (X Int b)
  deriving Text

data T a
  = E1

instance Eq a => Text (T a) where
    showsPrec = error "show"
    readsPrec = error "read"
