{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances
           , DataKinds, NoMonomorphismRestriction, UndecidableInstances
           , TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

module T19336 where

import GHC.TypeLits

class X a b where
  convert :: a -> b

instance X Int String where
  convert = show

instance X String String where
  convert = id

instance {-# OVERLAPPABLE #-} TypeError ('Text "Oops") => X a b where
  convert = error "unreachable"

type family F a where
  F String = String
  F Int    = String

convert_f :: X a (F a) => a -> a -> F a
convert_f _ = convert

----------

class Poly a where
  poly :: a

instance Poly String where
  poly = "hi"

instance Poly Int where
  poly = 2

----------

oops = convert_f poly
