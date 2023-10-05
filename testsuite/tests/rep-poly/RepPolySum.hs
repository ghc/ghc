{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module RepPolySum1 where

import GHC.Exts

foo :: forall (rep :: RuntimeRep) (a :: TYPE rep). () -> (# Int# | a #)
foo _ = (# | bar () #)
  where
    bar :: () -> a
    bar _ = undefined

-- This one crucially must also cause an error,
-- even though there are no representation-polymorphic values.
-- See GHC ticket #20277.
baz :: forall (rep :: RuntimeRep) (a :: TYPE rep). () -> (# Int# | a #)
baz _ = (# 17# | #)
