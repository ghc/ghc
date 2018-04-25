data T6 = MkT6Bad {-# UNPACK #-} !Int | MkT6Good {-# UNPACK #-} !Int deriving Show

{-# RULES

"T6" [1] forall x. MkT6Bad x = MkT6Good x
  #-}

main = do
  print (MkT6Bad 42) -- late rule
