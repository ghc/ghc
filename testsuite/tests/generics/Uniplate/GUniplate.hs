{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE IncoherentInstances        #-} -- necessary, unfortunately

module GUniplate where

import GHC.Generics

--------------------------------------------------------------------------------
-- Generic Uniplate
--------------------------------------------------------------------------------

class Uniplate' f b where
  children' :: f a -> [b]

instance Uniplate' U1 a where
  children' U1 = []

instance Uniplate' (K1 i a) a where
  children' (K1 a) = [a]

instance Uniplate' (K1 i a) b where
  children' (K1 _) = []

instance (Uniplate' f b) => Uniplate' (M1 i c f) b where
  children' (M1 a) = children' a

instance (Uniplate' f b, Uniplate' g b) => Uniplate' (f :+: g) b where
  children' (L1 a) = children' a
  children' (R1 a) = children' a

instance (Uniplate' f b, Uniplate' g b) => Uniplate' (f :*: g) b where
  children' (a :*: b) = children' a ++ children' b 


class Uniplate a where 
  children :: a -> [a]
  default children :: (Generic a, Uniplate' (Rep a) a) => a -> [a]
  children = children' . from


-- Base types instances
instance Uniplate Char  where children _ = []
instance Uniplate Int   where children _ = []
instance Uniplate Float where children _ = []

instance Uniplate [a] where
  children []    = []
  children (_:t) = [t]
