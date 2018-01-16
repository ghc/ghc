module Test where
data Boolean = FF | TT
data Pair a b = MkPair a b
data LList alpha = Nill | Conss alpha (LList alpha) 
data Nat = Zero | Succ Nat
data Tree x = Leaf x | Node (Tree x) (Tree x) 
data A a = MkA a (A a) 

append :: LList a -> LList a -> LList a
append  xs ys  = case xs of
                  Conss z zs  -> Conss z (append zs ys) 
                  v -> ys




