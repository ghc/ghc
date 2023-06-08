{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

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

type Eq :: TYPE rep -> Constraint
class Boolean (Logic a) => Eq (a :: TYPE rep) where
  type Logic (a :: TYPE rep) :: TYPE (Rep rep)
  (==) :: a -> a -> Logic a

type POrd :: TYPE rep -> Constraint
class Eq a => POrd (a :: TYPE rep) where
  inf :: a -> a -> a

type MinBound :: TYPE rep -> Constraint
class POrd a => MinBound (a :: TYPE rep) where
  minBound :: () -> a

type Lattice :: TYPE rep -> Constraint
class POrd a => Lattice (a :: TYPE rep) where
  sup :: a -> a -> a

type Bounded :: TYPE rep -> Constraint
class (Lattice a, MinBound a) => Bounded (a :: TYPE rep) where
  maxBound :: () -> a

type Complemented :: TYPE rep -> Constraint
class Bounded a => Complemented (a :: TYPE rep) where
  not :: a -> a

type Heyting :: TYPE rep -> Constraint
class Bounded a => Heyting (a :: TYPE rep) where
  infixr 3 ==>
  (==>) :: a -> a -> a

class (Complemented a, Heyting a) => Boolean a

(||) :: Boolean a => a -> a -> a
(||) = sup

(&&) :: Boolean a => a -> a -> a
(&&) = inf
