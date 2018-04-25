{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}

module T4903a where

class El phi ix where
  proof :: phi ix

class Fam phi where
  from :: phi ix -> ix -> PF phi I0 ix

type family PF (phi :: * -> *) :: (* -> *) -> * -> *

data I0 a = I0 a

data I xi      (r :: * -> *) ix = I (r xi)
data (f :*: g) (r :: * -> *) ix = f r ix :*: g r ix

class HEq phi f where
  heq :: (forall ix. phi ix -> r ix -> Bool)
      -> phi ix -> f r ix -> Bool

instance El phi xi => HEq phi (I xi) where
  -- Replacing proof by undefined solves the problem
  heq eq _ (I x)     = eq proof x

instance (HEq phi f, HEq phi g) => HEq phi (f :*: g) where
  -- The problem only arises when there are two calls to heq here
  heq eq p (x :*: y) = heq eq p x && heq eq p y


{-# INLINABLE eq #-}
eq :: (Fam phi, HEq phi (PF phi)) => phi ix -> ix -> Bool  
eq p x = heq (\p (I0 x) -> eq p x) p (from p x)


data Tree = Bin Tree Tree

tree :: Tree
-- The problem only occurs on an inifite (or very large) structure  
tree = Bin tree tree

data TreeF :: * -> * where Tree :: TreeF Tree

type instance PF TreeF = I Tree :*: I Tree
-- If the representation is only |I Tree| then there is no problem

instance Fam TreeF where
  from Tree (Bin l r) = I (I0 l) :*: I (I0 r)

instance El TreeF Tree where proof = Tree  
