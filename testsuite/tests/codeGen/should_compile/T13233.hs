{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}
module Bug where

import GHC.Exts (TYPE)

class Foo (a :: TYPE rep) where
  bar :: forall (b :: TYPE rep2). (a -> a -> b) -> a -> a -> b

baz :: forall (a :: TYPE rep). Foo a => a -> a -> (# a, a #)
baz = bar (#,#)
