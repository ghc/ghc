{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- #2721

module T2721 where

class C a where
  type T a
  foo :: a -> T a

instance C Int where
  type T Int = Int
  foo = id

newtype N = N Int deriving(C)
