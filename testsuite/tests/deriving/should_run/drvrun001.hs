-- Test newtype derived instances

newtype Age = MkAge Int deriving (Eq, Show)

instance Num Age where
  (+) (MkAge a) (MkAge b) = MkAge (a+b)
  (*)         = undefined
  negate      = undefined
  abs         = undefined
  signum      = undefined
  fromInteger = undefined

main = print (MkAge 3 + MkAge 5)
