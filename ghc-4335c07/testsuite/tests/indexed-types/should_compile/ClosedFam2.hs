{-# LANGUAGE TypeFamilies #-}

module ClosedFam2 where

import ClosedFam2a

type family Foo a where
  Foo Int = Bool
  Foo [a] = Maybe a
