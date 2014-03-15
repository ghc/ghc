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
  foo, bar, bat :: a

  -- | .. or just this
  fooBarBat :: (a,a,a)

  {-# MINIMAL (foo, bar) | (bar, bat) | (foo, bat) | fooBarBat #-}

class Weird a where
  a,b,c,d,e,f,g :: a

  {-# MINIMAL ((a, b), c | (d | (e, (f | g)))) #-}

class NoMins a where
  x,y,z :: a

  -- | Has a default implementation!
  z = x

class FullMin a where
  aaa,bbb :: a

class PartialMin a where
  ccc,ddd :: a

class EmptyMin a where
  eee,fff :: a
  eee = fff
  fff = undefined
