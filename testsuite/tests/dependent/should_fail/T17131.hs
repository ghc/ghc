{-# LANGUAGE MagicHash, UnboxedTuples, TypeInType, TypeFamilies, TypeOperators #-}

module T17131 where

import GHC.Exts

type family TypeReps xs where
   TypeReps '[]                 = '[]
   TypeReps ((a::TYPE k) ': as) = k ': TypeReps as

type family Tuple# xs = (t :: TYPE ('TupleRep (TypeReps xs))) where
   Tuple# '[a]                 = (# a #)
