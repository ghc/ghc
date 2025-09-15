{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
module T16059b where

type Foo = forall a. a
type Bar = (# #)
