{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T16030 where

import Data.Proxy

data Foo1 (a :: k) where
  MkFoo1a :: Proxy a -> Int -> Foo1 a
  MkFoo1b :: { a :: Proxy a, b :: Int } -> Foo1 a

data family Foo2 (a :: k)
data instance Foo2 (a :: k) where
  MkFoo2a :: Proxy a -> Int -> Foo2 a
  MkFoo2b :: { c :: Proxy a, d :: Int } -> Foo2 a
