{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module T10592 where

import Data.Kind (Type)
import Prelude (Bool(True,False),Integer,Ordering)
import qualified Prelude

--------------------
-- class hierarchy

class Boolean (Logic a) => Eq a where
    type Logic a :: Type
    (==) :: a -> a -> Logic a

    (/=) :: a -> a -> Logic a
    a/=b = not (a==b)

class Eq a => POrd a where
    inf :: a -> a -> a

class POrd a => MinBound a where
    minBound :: a

class POrd a => Lattice a where
    sup :: a -> a -> a

class (Lattice a, MinBound a) => Bounded a where
    maxBound :: a

class Bounded a => Complemented a where
    not :: a -> a

class Bounded a => Heyting a where
    infixr 3 ==>
    (==>) :: a -> a -> a

class (Complemented a, Heyting a) => Boolean a

(||) :: Boolean a => a -> a -> a
(||) = sup

(&&) :: Boolean a => a -> a -> a
(&&) = inf

--------------------
-- Bool instances
-- (these work fine)

instance Eq Bool where
    type Logic Bool = Bool
    (==) = (Prelude.==)

instance POrd Bool where
    inf True  True  = True
    inf _     _     = False

instance MinBound Bool where
    minBound = False

instance Lattice Bool where
    sup False False = False
    sup _     _     = True

instance Bounded Bool where
    maxBound = True

instance Complemented Bool where
    not True  = False
    not False = True

instance Heyting Bool where
    False ==> _ = True
    True  ==> a = a

instance Boolean Bool

--------------------
-- Integer instances
-- (these work fine)

instance Eq Integer where
    type Logic Integer = Bool
    (==) = (Prelude.==)

instance POrd Integer where
    inf = Prelude.min

instance Lattice Integer where
    sup = Prelude.max

--------------------
-- function instances
-- (these cause GHC to loop)

instance Eq b => Eq (a -> b) where
    type Logic (a -> b) = a -> Logic b
    f==g = \a -> f a == g a

instance POrd b => POrd (a -> b) where
    inf f g = \a -> inf (f a) (g a)

instance MinBound b => MinBound (a -> b) where
    minBound = \_ -> minBound

instance Lattice b => Lattice (a -> b) where
    sup f g = \a -> sup (f a) (g a)

instance Bounded b => Bounded (a -> b) where
    maxBound = \_ -> maxBound

instance Complemented b => Complemented (a -> b) where
    not f = \a -> not (f a)

instance Heyting b => Heyting (a -> b) where
    f ==> g = \a -> f a ==> g a

instance Boolean b => Boolean (a -> b)
