{-# LANGUAGE GADTs #-}
{-# OPTIONS -haddock -ddump-parsed-ast #-}

-- Haddock comments in this test case are all rejected.

module
  -- | Bad comment for the module
  T17544_kw (
  Foo(..),
  Bar(..),
  Cls(..)) where

data Foo -- | Bad comment for MkFoo
  where MkFoo :: Foo

newtype Bar -- | Bad comment for MkBar
  where MkBar :: () -> Bar

class Cls a
    -- | Bad comment for clsmethod
  where
    clsmethod :: a
