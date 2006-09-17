{-# OPTIONS -fglasgow-exts #-}

-- Here are a bunch of tests for impredicative polymorphism
-- mainly written by Dimitrios

module ShouldCompile where

xs :: [forall a. a->a]
xs = [\x -> x]

foo = id xs

-- Annotation resolves impredicative instantiation
bar = ((:)::(forall a.a ->a) -> [forall a. a->a] -> [forall a. a ->a])
               (head foo) foo

-- result type resolves everything! really neat
barr :: [forall a. a -> a]
barr = (head foo):(tail foo)

zoo = tail xs
zooo = head xs

-- This is the only unsatisfactory case...annotating
-- one of the arguments does not do the job...but maybe
-- this is reasonable to expect ...
-- bar3 = ((head foo) :: forall a. a ->a) : foo

data Pair a b where
   P :: a -> b -> Pair a b

data List a where
   Nil  :: List a
   Cons :: a -> List a -> List a
   FromMono :: (a->a) -> List (forall a. a->a)


f :: Int -> Pair Int Int
f x = P x x

h0 :: (forall a. a -> a) -> Int
h0 g = let y = P (g 3) (g (P 3 4))
      in 3


h1 (g::(forall a. a ->a))
   = let y = P (g 3) (g (P 3 4))
     in 3

h2 :: (forall a. a -> a) -> Int
h2 (g::(forall a. a ->a)) = let y = P (g 3) (g (P 3 4))
                            in 3

xs1 :: List (forall a. a ->a)
xs1 = let cons = Cons :: (forall a. a ->a)
		      -> List (forall a. a->a)
		      -> List (forall a. a ->a)
      in cons (\x -> x) Nil

xs2 :: List (forall a. a -> a)
xs2 = (Cons :: ((forall a. a->a)
	    -> List (forall a. a->a)
	    -> List (forall a. a->a)))
	(\x ->x) Nil

foo2 :: forall a. List a -> a -> a
foo2 x y = y

bar4 = (foo2 :: List (forall a. a->a) -> (forall a. a->a) -> (forall a.a->a)) 
	   xs1 (\x -> x)


