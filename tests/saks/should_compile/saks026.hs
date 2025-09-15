{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes, DataKinds, PolyKinds, GADTs, TypeFamilies #-}

module SAKS_026 where

import Data.Kind

data HigherRank (f :: forall x. x -> Type)

data P :: forall k. k -> Type

type PSyn :: forall k. k -> Type
type PSyn = (P :: forall k. k -> Type)

type Test = HigherRank PSyn
