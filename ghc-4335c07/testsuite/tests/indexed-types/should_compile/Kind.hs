{-# LANGUAGE TypeFamilies #-}

module Kind where

class C (a :: * -> *) where
  type T a

foo :: a x -> T a
foo = undefined

