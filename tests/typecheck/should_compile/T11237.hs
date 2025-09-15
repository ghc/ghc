{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE GADTs #-}
module T11237 where

import qualified Data.Kind

data Works :: Data.Kind.Type where
  WorksConstr :: Works

type Set = Data.Kind.Type

data ShouldWork :: Set where
  ShouldWorkConstr :: ShouldWork
