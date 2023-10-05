{-# LANGUAGE ExplicitForAll #-}

module T22537 where

import Data.Coerce

newtype M   = MkM (forall a b. a -> b -> b)
newtype N a = MkN (forall b. a -> b -> b)
newtype P   = MkP (forall a. N a)

g1 :: M -> P
g1 x = coerce x

g2 :: P -> M
g2 x = coerce x
