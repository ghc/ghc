{-# OPTIONS_HADDOCK hide #-}
module HiddenInstancesA where

-- | Should be visible
class Foo a

-- | Should be visible
data Bar

-- | Should be visible
instance Foo Bar

-- | Should *not* be visible
data Baz

-- | Should *not* be visible
instance Foo Baz
