{-# LANGUAGE RoleAnnotations #-}
module MapAsSet where

import Data.Set

type role Map nominal representational
data Map k a
instance Functor (Map k)

keysSet :: Map k a -> Set k
fromSet :: (k -> a) -> Set k -> Map k a
