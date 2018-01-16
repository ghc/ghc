{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module OversatDecomp where

class Blah f a where
  blah :: a -> T f f a

class A (f :: * -> *) where
  type T f :: (* -> *) -> * -> *

wrapper :: Blah f a => a -> T f f a
wrapper x = blah x
