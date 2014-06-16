{-# LANGUAGE TypeFamilies #-}

module ClosedFam2 where

import {-# SOURCE #-} ClosedFam2

type family Foo a where
  Foo Int = Bool
  Foo [a] = Maybe a