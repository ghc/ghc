-- | This tests the new MINIMAL pragma present in GHC 7.8
module Minimal
  ( Foo(..)
  , Weird(..)
  , NoMins(..)
  , FullMin(..)
  , PartialMin(ccc)
  , EmptyMin(..)
  ) where

class Foo a where
  -- | Any two of these are required...
  foo :: a
  bar :: a
  bat :: a

  -- | .. or just this
  fooBarBat :: (a,a,a)

  {-# MINIMAL (foo, bar) | (bar, bat) | (foo, bat) | fooBarBat #-}

class Weird a where
  a :: a
  b :: a
  c :: a
  d :: a
  e :: a
  f :: a
  g :: a

  {-# MINIMAL ((a, b), c | (d | (e, (f | g)))) #-}

class NoMins a where
  x :: a
  y :: a
  z :: a

  -- | Has a default implementation!
  z = x

class FullMin a where
  aaa :: a
  bbb :: a

class PartialMin a where
  ccc :: a
  ddd :: a

class EmptyMin a where
  eee :: a
  fff :: a
  eee = fff
  fff = undefined
