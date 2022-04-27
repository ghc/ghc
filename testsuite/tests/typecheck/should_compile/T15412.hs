{-# Language DataKinds, PolyKinds, TypeFamilies, UndecidableInstances #-}

module T15412 where

import Data.Kind

newtype I a = I a

type C = Constraint

type family
  UnitC :: C where
  UnitC = ()

instance UnitC => Functor I where
  -- The UnitC type family in the context needs UndecidableIntances
  fmap = undefined
