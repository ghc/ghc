{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module RepPolyTuple where

import GHC.Exts

foo :: forall (r :: RuntimeRep) (a :: TYPE r). () -> (# a, a, a #)
foo _ = (# bar (), bar (), bar () #)
  where
    bar :: forall (r :: RuntimeRep) (a :: TYPE r). () -> a
    bar _ = undefined
