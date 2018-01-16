-- !! TEST OF DEFACTORISATION FOR FUNCTIONS THAT DROP 
-- !!             POLYMORPHIC VARIABLES

module Test where
data Boolean = FF | TT
data Pair a b = MkPair a b
data LList alpha = Nill | Conss alpha (LList alpha) 
data Nat = Zero | Succ Nat
data Tree x = Leaf x | Node (Tree x) (Tree x) 
data A a = MkA a (A a) 

append :: LList a -> LList a -> LList a
append  xs ys  = case xs of
                  Nill -> ys 
                  Conss z zs  -> Conss z (append zs ys) 

-- The following function drops @b@.

flat :: Tree (Pair a b) -> LList a
flat t =  case t of
              Leaf (MkPair a b) -> Conss a Nill 
              Node l r -> append (flat l) (flat r)

fl :: Boolean -> LList Boolean
fl x = flat (Leaf (MkPair TT Zero)) 


