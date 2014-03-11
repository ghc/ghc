{-# LANGUAGE TypeFamilies #-}
-- This tests what happens if we have unexported types
-- in type instances. The expected behaviour is
-- that we get the instance, Y is not linked and
-- Haddock shows a linking warning.
--
-- The other families and instances that are not exported should not
-- show up at all
module TypeFamilies2 (X, Foo, Bar) where

data X
data Y

type family Foo a
type instance Foo X = Y
type instance Foo Y = X -- Should be hidden

data family Bar a
data instance Bar X = BarX Y

type family Invisible a
type instance Invisible X = Y
type instance Invisible Y = X
