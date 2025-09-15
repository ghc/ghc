{-# LANGUAGE GADTs, UnliftedNewtypes, StandaloneKindSignatures, RankNTypes, TypeFamilies, PolyKinds #-}

module T18891 where

import GHC.Exts( TYPE )

data family T2 (a :: k)
data instance T2 a where
  MkT2 :: T2 Maybe

newtype N3 :: forall k -> TYPE k where
  MkN3 :: N3 m -> N3 m

type N4 :: forall k -> TYPE k
newtype N4 :: forall k -> TYPE k where
  MkN4 :: N4 m -> N4 m
