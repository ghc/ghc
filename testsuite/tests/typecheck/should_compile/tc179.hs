{-# LANGUAGE ExistentialQuantification, FlexibleInstances,
             OverlappingInstances, UndecidableInstances #-}

-- Tests context reduction for existentials

module TestWrappedNode where

class Foo a where { op :: a -> Int }

instance Foo a => Foo [a] where  	-- NB overlap
  op (x:xs) = op x
instance Foo [Int] where		-- NB overlap
  op x = 1

data T = forall a. Foo a => MkT a

f :: T -> Int
f (MkT x) = op [x,x]
	-- The op [x,x] means we need (Foo [a]).  We used to 
	-- complain, saying that the choice of instance depended on 
	-- the instantiation of 'a'; but of course it isn't *going* 
	-- to be instantiated.

