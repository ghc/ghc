{-# LANGUAGE GADTs, DataKinds, PolyKinds, ExplicitForAll #-}

module BadTelescope2 where

import Data.Kind

data SameKind :: k -> k -> *

-- This declaration made GHC 8.10 produce a Core Lint error
data T a b where
  MkT :: forall a kx (b :: kx). SameKind a b -> T a b


