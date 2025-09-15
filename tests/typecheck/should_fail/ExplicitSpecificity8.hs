{-# LANGUAGE RankNTypes, PolyKinds, GADTs #-}

module ExplicitSpecificity8 where

import GHC.Types

data T1 :: forall k -> k -> Type

data T2 :: forall {k} -> k -> Type

foo1 :: T1 Type Int -> ()
foo1 _ = ()

foo2 :: T2 Type Int -> ()
foo2 _ = ()
