{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE RankNTypes #-}

module T3692 where

type Foo a b = () -> (Bar a => a)

class Bar a where {}

boo :: Foo p q
boo x = undefined

foo :: Foo a b
foo y = id (\x -> boo x) y
