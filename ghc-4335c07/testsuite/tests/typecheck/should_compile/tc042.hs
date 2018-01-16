-- !!! a file mailed us by Ryzard Kubiak. This provides a good test of the code
-- !!! handling type signatures and recursive data types.

module ShouldSucceed where

data Boolean = FF | TT
data Pair a b = Mkpair a b
data List alpha = Nil | Cons alpha (List alpha)
data Nat = Zero | Succ Nat
data Tree t = Leaf t | Node (Tree t) (Tree t) 

idb :: Boolean -> Boolean
idb x = x      


swap :: Pair a b -> Pair b a
swap t = case t of 
           Mkpair x y -> Mkpair y x 

neg :: Boolean -> Boolean
neg b = case b of 
          FF -> TT 
          TT -> FF 

nUll :: List alpha -> Boolean
nUll l = case l of 
           Nil -> TT
           Cons y ys  -> FF 

idl ::  List a -> List a
idl xs  = case xs of
           Nil -> Nil 
           Cons y ys -> Cons y (idl ys)
     
add :: Nat -> Nat -> Nat
add a b = case a of 
            Zero -> b 
            Succ c -> Succ (add c b) 

app :: List alpha -> List alpha -> List alpha
app  xs zs  = case xs of
                  Nil -> zs 
                  Cons y ys  -> Cons y (app ys zs) 
                 
lEngth :: List a -> Nat
lEngth xs = case xs of 
              Nil -> Zero  
              Cons y ys  -> Succ(lEngth ys) 

before :: List Nat -> List Nat
before xs = case xs of
              Nil -> Nil
              Cons y ys -> case y of 
                              Zero -> Nil
                              Succ n -> Cons y (before ys)       

rEverse :: List alpha -> List alpha
rEverse rs = case rs of
               Nil -> Nil
               Cons y ys -> app (rEverse ys) (Cons y Nil) 
             

flatten :: Tree alpha -> List alpha
flatten t = case t of
              Leaf x   -> Cons x Nil 
              Node l r -> app (flatten l) (flatten r)
         
sUm :: Tree Nat -> Nat
sUm t = case t of
          Leaf t   -> t
          Node l r -> add (sUm l) (sUm r) 


