{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module T5359b where

-----------------------------------------------------------------------------
-- Base
-----------------------------------------------------------------------------
infixr 5 :+:
infixr 6 :*:

data U       = U              
data a :+: b = L a | R b      
data a :*: b = a :*: b        
newtype Rec a   = Rec a       

class Representable a where
  type Rep a
  to   :: Rep a -> a
  from :: a -> Rep a


data Tree = Leaf | Bin Int Tree Tree

instance Representable Tree where
  type Rep Tree =     U
                      :+: (Rec Int :*: Rec Tree :*: Rec Tree)

  from (Bin x l r) = R ((Rec x :*: Rec l :*: Rec r))
  from Leaf        = L (U)

  to (R ((Rec x :*: (Rec l) :*: (Rec r)))) = Bin x l r
  to (L (U))                               = Leaf

--------------------------------------------------------------------------------
-- Generic enum
--------------------------------------------------------------------------------

class Enum' a where
  enum' :: [a]

instance Enum' U where enum' = undefined
instance (Enum' a) => Enum' (Rec a) where enum' = undefined
instance (Enum' f, Enum' g) => Enum' (f :+: g) where enum' = undefined
instance (Enum' f, Enum' g) => Enum' (f :*: g) where enum' = undefined


-- This INLINE pragma is essential for the bug
{-# INLINE genum #-}
genum :: (Representable a, Enum' (Rep a)) => [a]
-- The definition of genum is essential for the bug
genum = map to enum'


instance Enum' Tree where enum' = genum
instance Enum' Int  where enum' = []

-- This SPECIALISE pragma is essential for the bug
{-# SPECIALISE genum :: [Tree] #-}
