{-# LANGUAGE TypeFamilies #-}
module T20260 where

data Bar

type Foo :: *
type family Foo where
  Bar = ()
