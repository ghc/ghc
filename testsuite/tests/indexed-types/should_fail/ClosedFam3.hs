{-# LANGUAGE TypeFamilies #-}

module ClosedFam3 where

import {-# SOURCE #-} ClosedFam3

type family Foo a where
  Foo Int = Bool
  Foo Double = Char

type family Bar a where
  Bar Int = Bool
  Bar Double = Double

type family Baz a where
  Baz Int = Bool