data Z a b
  = C1 (T a)
  | C2 (Z [a] [b])
  deriving Text

data T a
  = E1

instance Eq a => Text (T a) where
    showsPrec = error "show"
    readsPrec = error "read"
