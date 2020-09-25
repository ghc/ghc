-- Check that nothing goes wrong with UNPACK in recursive case.
data T = MkT {-# UNPACK #-} !(Maybe T)
  deriving Show

t :: T
t = MkT (Just t)

main = print $ take 100 (show t)
