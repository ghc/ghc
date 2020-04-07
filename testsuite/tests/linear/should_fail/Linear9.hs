{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Linear9 where

fst :: (a,b) -> a
fst (a,_) = a

incorrectFst :: (a,b) ⊸ a
incorrectFst (a,_) = a

incorrectFstVar :: (a,b) ⊸ a
incorrectFstVar (a,b) = a

incorrectFirstDup :: (a,b) ⊸ ((a,a),b)
incorrectFirstDup (a,b) = ((a,a),b)

incorrectFstFst :: ((a,b),c) ⊸ a
incorrectFstFst ((a,_),_) = a

data Test a
  = Foo a a
  | Bar a

incorrectTestFst :: Test a ⊸ a
incorrectTestFst (Foo a _) = a
incorrectTestFst (Bar a)   = a
