module Test where
data Boolean = FF | TT
data Pair a b = MkPair a b
data LList alpha = Nill | Conss alpha (LList alpha) 
data Nat = Zero | Succ Nat
data Tree x = Leaf x | Node (Tree x) (Tree x) 
data A a = MkA a (A a) 

{-
map :: (a -> b) -> [a] -> [b]
map f xs = case xs of
             []     -> []
             (y:ys) -> (f y):(map f ys)

map_ide :: [[a]] -> [[a]]
map_ide = map (\x->x)
-}

my_id :: a -> a
my_id x = x

idNat :: Nat -> Nat
idNat x = x

idBool :: Boolean -> Boolean
idBool x = x

fun :: (a->b) -> a -> b
fun f x = g f
 where 
  g f   = f x

