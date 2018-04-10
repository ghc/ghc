{-# LANGUAGE TypeInType, KindSignatures, ExplicitForAll, RankNTypes #-}

module T11635 where

import Data.Kind

data X (a :: forall k. k -> * ) b = X
