module Test where
data Boolean = FF | TT
data Pair a b = Mkpair a b
data LList alpha = Nill | Conss alpha (LList alpha) 
data Nat = Zero | Succ Nat
data Tree t = Leaf t | Node (Tree t) (Tree t) 
data A a = MkA a (A a) 
data Foo baz = MkFoo (Foo (Foo baz))
{-
 append1 :: LList a -> LList a -> LList a
 append1 xs ys = append2 xs
   where
     append2 xs = case xs of
                    Nill -> ys
                    Conss x xs -> Conss x (append3 xs)
     append3 xs = case xs of
                    Nill -> ys
                    Conss x xs -> Conss x (append2 xs)

 loop :: a -> a
 loop x =  loop x

 hd :: LList b -> b
 hd Nill         = loop
 hd (Conss y ys) = y

 hdb :: LList (LList b) -> LList b
 hdb = hd

 append :: [a] -> [a] -> [a]
 append [] ys     = ys
 append (x:xs) ys = x:(append xs ys)
 
 f :: [a] -> [a]
 f y = append x (f y)
   where  x = append x (f y)
-}
app :: LList a -> LList a -> LList a
app Nill Nill = Nill
app  xs ys  = case xs of
                Nill -> ys 
                Conss z zs  -> Conss z (app zs ys) 
{-
 app :: LList a -> LList a -> LList a
 app  xs ys = case xs of
               Nill -> case ys of
                        Nill -> Nill
                        Conss u us -> ap 
               Conss a as -> ap 
  where ap = case xs of
              Nill -> ys 
              Conss z zs  -> Conss z (app zs ys) 

 app :: LList a -> LList a -> LList a
 app  xs ys = case xs of
               Nill -> case ys of
                        Nill -> Nill
                        Conss u us -> ap xs ys
               Conss a as -> ap xs ys

 ap xs ys = case xs of
              Nill -> ys 
              Conss z zs  -> Conss z (app zs ys) 

 ap :: LList a -> LList a -> LList a
 ap  xs ys  = case xs of
                 Nill -> ys 
                 Conss z zs  -> Conss z (ap zs ys) 

 app :: LList a -> LList a -> LList a
 app  xs ys = case xs of
               Nill -> case ys of
                        Nill -> Nill
                        Conss u us -> ap xs ys
               Conss a as -> ap xs ys
-}
