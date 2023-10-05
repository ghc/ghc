{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T15341 where

type family Foo (a :: k) :: k where
  Foo a = a
