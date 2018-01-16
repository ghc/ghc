{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module T6147 where

data family T a
data instance T Int = T_Int Int

class C a where
  foo :: a -> T a

instance C Int where
  foo = T_Int

newtype Foo = Foo Int deriving(C)
