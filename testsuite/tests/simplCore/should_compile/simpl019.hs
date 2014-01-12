{-# LANGUAGE GADTs, FlexibleInstances #-}
{-# OPTIONS_GHC -O2 #-}

-- See Trac #1746

module Foo where

data T a where T :: T a -> T [a]

class C a where
  f :: a -> ()

instance C (T [a]) where
  f (T x@(T _)) = f x
