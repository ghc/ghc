{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}
module Bug where

import GHC.Exts ( TYPE, RuntimeRep )

class Foo (a :: TYPE rep) where
  bar :: forall rep2 (b :: TYPE rep2). (a -> a -> b) -> a -> a -> b

baz :: forall rep (a :: TYPE rep). Foo a => a -> a -> (# a, a #)
baz = bar (#,#)

obscure :: (forall (rep1 :: RuntimeRep) (rep2 :: RuntimeRep)
                   (a :: TYPE rep1) (b :: TYPE rep2).
                   a -> b -> (# a, b #)) -> ()
obscure _ = ()

quux :: ()
quux = obscure (#,#)
