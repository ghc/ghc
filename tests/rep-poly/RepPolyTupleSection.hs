{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module RepPolyTupleSection where

import GHC.Exts

foo :: forall (r :: RuntimeRep) (a :: TYPE r). a -> (# Int#, a #)
foo = (# 3#, #)
