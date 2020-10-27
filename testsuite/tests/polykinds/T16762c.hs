{-# LANGUAGE TypeFamilies, GADTs, DataKinds, PolyKinds, ExplicitForAll #-}

module Foo where

import Data.Kind

data SameKind :: k -> k -> Type

-- Bad telescope
data T = forall a k (b::k). MkT (SameKind a b)
