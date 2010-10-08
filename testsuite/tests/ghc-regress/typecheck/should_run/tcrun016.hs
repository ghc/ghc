{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances #-}

-- !!! Functional dependencies
-- This one made the 5.00.1 typechecker go into an infinite loop.
-- The context simplifier keep thinking it was doing an 'improve'
-- step, and hence kept going round and round.

module Main where

main = print (get ((AttributeLeaf (MyLabel "x") 4)::Env1) (MyLabel "x"))

class Eq l => Domain d l | d -> l where
	(<<) :: d -> d -> d
	empty :: d
class Domain e l => Environment e l t | e -> l t where
	get :: e -> l -> Maybe t
	attribute :: l -> t -> e

class Eq' a where
	(=?=) :: a -> a -> Bool

newtype MyLabel = MyLabel String deriving Eq

instance Eq' MyLabel where
	l =?= l' = l == l'

data BinTreeEnv l t = 
	EmptyEnv | 
	AttributeLeaf l t | 
	Union (BinTreeEnv l t) (BinTreeEnv l t)

instance (Eq l, Eq' l) => Domain (BinTreeEnv l t) l where
	EmptyEnv << d = d
	d << EmptyEnv = d
	d << d' = Union d d'
	empty = EmptyEnv

instance (Eq l, Eq' l) => Environment (BinTreeEnv l t) l t where
	get EmptyEnv l = Nothing
	get (AttributeLeaf l t) l' = if l =?= l' then Just t
				     else Nothing
	get (Union d d') l = error "!??"

        attribute l t = AttributeLeaf l t

type Env1 = BinTreeEnv MyLabel Integer

