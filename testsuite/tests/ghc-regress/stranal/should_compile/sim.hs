module Test where
data Boolean = FF | TT
data Pair a b = MkPair a b
data LList alpha = Nill | Conss alpha (LList alpha) 
data Nat = Zero | Succ Nat
data Tree x = Leaf x | Node (Tree x) (Tree x) 
data A a = MkA a (A a) 
{-
id :: a -> a
id x = x      

idb :: Boolean -> Boolean
idb b = b

swap :: Pair a b -> Pair b a
swap t = case t of 
           MkPair x y -> MkPair y x

bang :: A (A a) -> Boolean
bang x = case x of
           MkA y ys -> TT

neg :: Boolean -> Boolean
neg b = case b of 
         FF -> TT 
         TT -> FF 

null :: LList x -> Boolean
null l = case l of 
           Nill -> TT
           _ -> FF

loop :: Boolean -> a
loop b = loop b
-}
idl ::  LList a -> LList a
idl xs  = case xs of
           Conss y ys -> Conss y (idl ys)
           _ -> Nill 
{-
idn :: Nat -> Nat
idn n = case n of
          Zero -> Zero 
          Succ m -> Succ (idn m)

add :: Nat -> Nat -> Nat
add a b = case a of 
            Zero -> b
            Succ c -> Succ (add c b) 

length :: LList a -> Nat
length xs = case xs of 
              Nill -> Zero
              Conss y ys  -> Succ(length ys) 

before :: LList Nat -> LList Nat
before xs = case xs of
              Nill -> Nill
              Conss y ys -> case y of 
                             Zero -> Nill
                             Succ n -> Conss y (before ys)     

reverse :: LList a -> LList a
reverse rs = case rs of
               Nill -> Nill
               Conss y ys -> append (reverse ys) (Conss y Nill) 

f :: Nat -> Nat
f n = case n of
        Zero -> Zero
        Succ m -> Succ (g m)

g :: Nat -> Nat
g n = case n of
       Zero -> Zero
       Succ m -> Succ (f m)

append :: LList a -> LList a -> LList a
append  xs ys  = case xs of
                  Nill -> ys 
                  Conss z zs  -> Conss z (append zs ys) 

flatten :: Tree alpha -> LList alpha
flatten t = case t of
              Leaf x   -> Conss x Nill 
              Node l r -> append (flatten l) (flatten r)

sum :: Tree Nat -> Nat
sum t = case t of
          Leaf t   -> t
          Node l r -> add (sum l) (sum r) 

suml :: LList Nat -> Nat
suml Nill = Zero
suml (Conss n ns) = add n (suml ns)

map :: (a -> b) -> LList a -> LList b
map f xs = case xs of
             Nill -> Nill
             Conss y ys -> Conss (f y) (map f ys)
-}


