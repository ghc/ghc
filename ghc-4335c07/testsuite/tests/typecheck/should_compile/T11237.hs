{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
module TypeInTypeBug where

import qualified Data.Kind

data Works :: Data.Kind.Type where
  WorksConstr :: Works

type Set = Data.Kind.Type

data ShouldWork :: Set where
  ShouldWorkConstr :: ShouldWork
