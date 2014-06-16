{-# LANGUAGE TypeFamilies #-}

module ClosedFam1 where

import {-# SOURCE #-} ClosedFam1

type family Foo a where
  Foo Int = Bool
  Foo [a] = Maybe a