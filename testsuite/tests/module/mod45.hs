{-# LANGUAGE Haskell2010 #-}
-- !!! Type sigs in instance decl
module M where
data T = T Int
instance Eq T where
  (==) :: T -> T -> Bool
  T x == T y = x == y

