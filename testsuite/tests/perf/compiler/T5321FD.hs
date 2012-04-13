{-# OPTIONS_GHC -fcontext-stack=1000 #-} 
{-# LANGUAGE 
     FlexibleContexts, FlexibleInstances, FunctionalDependencies, 
     MultiParamTypeClasses, OverlappingInstances, TypeSynonymInstances, 
     TypeOperators, UndecidableInstances, TypeFamilies #-} 
module T5321FD where

-------- USES FUNCTIONAL DEPENDENCIES -------------

-- Our running example, for simplicity's sake, is a type-level map of a
-- single function. For reference, here is the code for a simple
-- value-level map of a single function.

-- vfoo = id
-- mapfoo (x : xs) = vfoo x : mapfoo xs
-- mapfoo [] = []

-- Because Haskell is a lazy language, this runs in O(n) time and constant stack.

-- We now lift map to the type level, to operate over HLists.

-- First, the basic HList types

infixr 3 :*
data x :* xs = x :* xs deriving Show
data HNil = HNil deriving Show

-- Next, a large boring HList

-- Adds ten cells
addData x = i :* i :* d :* d :* s :* 
            i :* i :* d :* d :* s :* 
            x 
    where i = 1 :: Int 
          d = 1 :: Double 
          s = "" 

-- Has 70 cells. 
sampleData = addData $ addData $ addData $ addData $ addData $ 
             addData $ addData $ 
             HNil

-- Next, a simple polymorphic function to map

class Foo x y | x -> y 
    where foo :: x -> y 
          foo = undefined

instance Foo Int Double
instance Foo Double Int
instance Foo String String

------------------------
-- Now, our map

class HMapFoo1 as bs | as -> bs where 
    hMapFoo1 :: as -> bs

instance (Foo a b, HMapFoo1 as bs) => HMapFoo1 (a :* as) (b :* bs) where 
    hMapFoo1 (x :* xs) = foo x :* hMapFoo1 xs

instance HMapFoo1 HNil HNil where 
    hMapFoo1 _ = HNil

-- If we enable the following line, compilation time is ~ 9 seconds.

testHMapFoo1 = hMapFoo1 sampleData 


------------------------
class HMapFoo2 acc as bs | acc as -> bs where 
    hMapFoo2 :: acc -> as -> bs

instance (Foo a b, HMapFoo2 (b :* bs) as res) => HMapFoo2 bs (a :* as) res where 
    hMapFoo2 acc (x :* xs) = hMapFoo2 (foo x :* acc) xs

instance HMapFoo2 acc HNil acc where 
    hMapFoo2 acc _ = acc

-- If we enable the following line, compilation time is a much more satisfying ~0.5s.

testHMapFoo2 = hMapFoo2 HNil sampleData 

------------------------
-- But wait, there's trouble on the horizon! Consider the following version: 

class HMapFoo3 acc as bs | acc as -> bs where 
    hMapFoo3 :: acc -> as -> bs

instance (HMapFoo3 (b :* bs) as res, Foo a b) => HMapFoo3 bs (a :* as) res where 
    hMapFoo3 acc (x :* xs) = hMapFoo3 (foo x :* acc) xs

instance HMapFoo3 acc HNil acc where 
    hMapFoo3 acc _ = acc

-- The only difference between hMapFoo2 and hMapFoo3 is that the order of
-- constraints on the inductive case has been reversed, with the
-- recursive constraint first and the immediately checkable constraint
-- second. Now, if we enable the following line, compilation time rockets
-- to ~6s!

testHMapFoo3 = hMapFoo3 HNil sampleData 
