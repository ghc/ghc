{-# LANGUAGE RankNTypes, QuantifiedConstraints, ConstraintKinds  #-}
-- NB: disabling these if enabled:
{-# LANGUAGE NoUndecidableInstances, NoUndecidableSuperClasses #-}

module T15316 where

import Data.Proxy

{-
class Class a where
         method :: a

subsume :: (Class a => Class b) => Proxy a -> Proxy b -> ((Class a => Class b) => r) -> r
subsume _ _ x = x

value :: Proxy a -> a
value p = subsume p p method
-}

subsume' :: Proxy c -> ((c => c) => r) -> r
subsume' _ x = x
