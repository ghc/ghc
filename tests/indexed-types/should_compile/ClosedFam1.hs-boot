{-# LANGUAGE TypeFamilies #-}

module ClosedFam1 where

type family Foo b where
  Foo Int = Bool
  Foo [different] = Maybe different