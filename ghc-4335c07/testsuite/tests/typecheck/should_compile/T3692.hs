{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE RankNTypes #-}

module T3692 where

type Foo a b = () -> (Bar a => a)

class Bar a where {}

foo :: Foo a b
foo = id (undefined :: Foo p q)
