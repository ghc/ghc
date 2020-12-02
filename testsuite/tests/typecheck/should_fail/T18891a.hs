{-# LANGUAGE GADTs, UnliftedNewtypes, StandaloneKindSignatures, RankNTypes, TypeFamilies, PolyKinds #-}

module T18891 where

import GHC.Exts( TYPE )

newtype N1 :: forall k. TYPE k where
   MkN1 :: N1 -> N1

type N2 :: forall k. TYPE k
newtype N2 :: forall k. TYPE k where
  MkN2 :: N2 -> N2

