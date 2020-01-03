{-# LANGUAGE TypeFamilies #-}

module T17633 where

type family Bar (a :: *) :: * where
  Bar Int = ()


type family Foo (a :: *) :: * where
  Bar Int = Bool
  Bar Bool = Int
  Bar a = a

