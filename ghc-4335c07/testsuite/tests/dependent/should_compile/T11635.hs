{-# LANGUAGE TypeInType, KindSignatures, ExplicitForAll #-}

module T11635 where

import Data.Kind

data X (a :: forall k. k -> * ) b = X
