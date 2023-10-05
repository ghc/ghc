{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module T16059a where

type Foo b = Eq b => b

f :: forall b (a :: Eq b => b). Int
f = 27

g :: forall b (a :: Foo b). Int
g = 42
