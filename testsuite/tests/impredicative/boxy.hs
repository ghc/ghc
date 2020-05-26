{-# LANGUAGE ImpredicativeTypes, ScopedTypeVariables, TypeApplications #-}

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

ids :: [forall a. a -> a]
ids = [id1,id1]

t1 :: [forall a. a -> a]
t1 = tail ids

t2 :: [forall a. a -> a]
t2 = sing id

t3 :: forall a. a -> a
t3 = head ids

{--------------- Examples from QMLF paper -------------------}

qF :: (forall a. a -> a -> a) -> (Bool, Char)
qF choose = (choose True False, choose 'a' 'b')

qG :: (forall a. a -> a -> a) -> (forall a. a -> a) -> (forall g. (g -> g) -> (g -> g))
qG choose id = choose id

qH :: (forall a. a -> a -> a) -> (forall a. a -> a) -> (forall b. b -> b) -> (forall b. b -> b)
qH choose id = choose id

choose :: forall a. a -> a -> a
choose x y = x

impred1 :: (Bool, Char)
impred1 = ($) qF choose  --- impredicative instantiation for $

impred2 :: (forall a. a -> a -> a) -> (Bool, Char)
impred2 = id qF

{------ Examples for Garrique/Remy paper -------}

--- all of these currently work in GHC with higher-rank types

self1 :: (forall a. a -> a) -> (forall a. a -> a)
self1 f = f f

self2 :: (forall a. a -> a) -> b -> b
self2 f = f f

gr1 = self1 id

gr2 = self2 id

gr3 = (\(u :: (forall a. a -> a) -> (forall a. a -> a)) -> u id) self1

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
