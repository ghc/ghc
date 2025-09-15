{-# LANGUAGE RankNTypes, QuantifiedConstraints, ConstraintKinds, UndecidableInstances #-}

-- Should produce a compile time "Reduction stack overflow" error
module T15316A where

import Data.Proxy

class Class a where
         method :: a

subsume :: (Class a => Class b) => Proxy a -> Proxy b -> ((Class a => Class b) => r) -> r
subsume _ _ x = x

value :: Proxy a -> a
value p = subsume p p method

