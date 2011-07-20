{-# OPTIONS_GHC -XImpredicativeTypes -fno-warn-deprecated-flags -XScopedTypeVariables #-}

module ShouldCompile where


{-------- Examples from the paper ---------}

f :: (forall a. [a] -> a) -> (Int, Char)
f get = (get [1,2], get ['a', 'b', 'c'])

g :: Maybe (forall a. [a] -> a) -> (Int, Char)
g Nothing = (0, '0')
g (Just get) = (get [1,2], get ['a','b','c'])

sing x = [x] 

id1 :: forall a. a -> a
id1 = id

{-
ids :: [forall a. a -> a]  
ids = [id1,id1] 

t1 :: [forall a. a -> a]
t1 = tail ids

t2 :: [forall a. a -> a]
t2 = sing id

t3 :: forall a. a -> a
t3 = head ids
-}

{--------------- Examples from QMLF paper -------------------}

qF :: (forall a. a -> a -> a) -> (Bool, Char)
qF choose = (choose True False, choose 'a' 'b')

qG :: (forall a. a -> a -> a) -> (forall a. a -> a) -> (forall g. (g -> g) -> (g -> g))
qG choose id = choose id

qH :: (forall a. a -> a -> a) -> (forall a. a -> a) -> (forall b. b -> b) -> (forall b. b -> b)
qH choose id = choose id
   
choose :: forall a. a -> a -> a
choose x y = x

{-
impred1 :: (Bool, Char)
impred1 = qF $ choose  --- impredicative instantiation for $

impred2 :: (forall a. a -> a -> a) -> (Bool, Char)
impred2 = id qF
-}

{------ Examples for Garrique/Remy paper -------}

--- all of these currently work in GHC with higher-rank types

self1 :: (forall a. a -> a) -> (forall a. a -> a)
self1 f = f f 

self2 :: (forall a. a -> a) -> b -> b
self2 f = f f 

gr1 = self1 id

gr2 = self2 id

gr3 = (\(u :: (forall a. a -> a) -> (forall a. a -> a)) -> u id) self1

{------------ Church numerals ----------}

type Church = forall a. a -> (a -> a) -> a

z :: Church
z = \z -> \f -> z

s :: Church -> Church
s = \n -> \z -> \f -> f (n z f)

fold :: Church -> a -> (a -> a) -> a
fold n f z = n f z

{-
mul :: Church -> Church -> Church
mul m n = \f -> \a -> m (n f) a

exp :: Church -> Church -> Church
exp m n = n (mul m) (s z)

idC :: Church -> Church
idC x = fold x s z

inc :: Church -> Church
inc x = fold x s (s z)
-}

{------- fix for higher rank types ---------}

data Tree a = Branch a (Tree (a,a)) | Leaf

type MapTree = forall a b. (a -> b) -> Tree a -> Tree b
cross f (a,b) = (f a,f b)

-- I think this should work in GHC now, but it doesn't
-- fix specialized to higher-rank type
fixMT :: (MapTree -> MapTree) -> MapTree
fixMT f = f (fixMT f)

mapTree' = fixMT (\ (mapTree :: MapTree) -> \f tree -> case tree of
 			    Branch a t -> Branch (f a) (mapTree (cross f) t)
 			    Leaf -> Leaf)

-- polymorphic fix
fix :: (a -> a) -> a
fix f = f (fix f)


-- mapTree'' :: MapTree
mapTree'' = (fix :: (MapTree -> MapTree) -> MapTree)
	       (\ mapTree -> \f tree -> case tree of
 			    Branch a t -> Branch (f a) (mapTree (cross f) t)
 			    Leaf -> Leaf)
