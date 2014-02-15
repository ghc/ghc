{-# LANGUAGE TypeFamilies #-}
-- This tests what happens if we have unexported types
-- in type instances. The expected behaviour is
-- that we get the instance, Y is not linked and
-- Haddock shows a linking warning.
module TypeFamilies2 (X, Foo, Bar) where

data X
data Y

type family Foo a
type instance Foo X = Y

data family Bar a
data instance Bar X = BarX Y
