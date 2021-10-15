{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module T20277 where

import GHC.Exts

-- This one crucially must cause an error,
-- even though there are no representation-polymorphic values.
-- See GHC ticket #20277.
baz :: forall (rep :: RuntimeRep) (a :: TYPE rep). () -> (# Int# | a #)
baz _ = (# 17# | #)
