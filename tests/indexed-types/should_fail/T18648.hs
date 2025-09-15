{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeFamilies #-}
module T18648 where

class Foo1 a where
   type Bar1 a
   type Bar1 (f a) = Bar1 a

class Foo2 a where
   type Bar2 a
   type Bar2 (Bar2 a) = a
