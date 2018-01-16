{-# LANGUAGE TypeFamilies #-}

module ClosedFam1 where

import ClosedFam1a

type family Foo a where
  Foo Int = Bool
  Foo [a] = Maybe a
