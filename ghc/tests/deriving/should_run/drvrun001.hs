-- Test newtype derived instances

newtype Age = MkAge Int deriving (Eq, Show)

instance Num Age where
  (+) (MkAge a) (MkAge b) = MkAge (a+b)

main = print (MkAge 3 + MkAge 5)
