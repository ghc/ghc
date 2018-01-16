{-# LANGUAGE TypeFamilies, DataKinds, TypeInType #-}

import GHC.Exts
import Prelude (Bool(True,False),Integer,Ordering,undefined)
import qualified Prelude
import Data.Kind

--------------------
-- class hierarchy

type family
  Rep (rep :: RuntimeRep) :: RuntimeRep where
  -- Rep IntRep         = IntRep
  -- Rep DoubleRep      = IntRep
  -- Rep PtrRepUnlifted = IntRep
  -- Rep PtrRepLifted   = PtrRepLifted

class Boolean (Logic a) => Eq (a :: TYPE rep) where
  type Logic (a :: TYPE rep) :: TYPE (Rep rep)
  (==) :: a -> a -> Logic a

class Eq a => POrd (a :: TYPE rep) where
  inf :: a -> a -> a

class POrd a => MinBound (a :: TYPE rep) where
  minBound :: () -> a

class POrd a => Lattice (a :: TYPE rep) where
  sup :: a -> a -> a

class (Lattice a, MinBound a) => Bounded (a :: TYPE rep) where
  maxBound :: () -> a

class Bounded a => Complemented (a :: TYPE rep) where
  not :: a -> a

class Bounded a => Heyting (a :: TYPE rep) where
  infixr 3 ==>
  (==>) :: a -> a -> a

class (Complemented a, Heyting a) => Boolean a

(||) :: Boolean a => a -> a -> a
(||) = sup

(&&) :: Boolean a => a -> a -> a
(&&) = inf
