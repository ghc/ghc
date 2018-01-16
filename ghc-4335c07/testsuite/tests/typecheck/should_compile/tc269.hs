{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
module Tc269 where

import GHC.Types

{-
-- We'd like this to kind check, but it doesn't today,
-- see Note [Missed opportunity to retain higher-rank kinds]

-- TSyn is in an SCC of its own, so we can read off the
-- kind directly.
data T (p :: forall k. k -> Type) = T
type TSyn = T
-}

-- S and SSyn are in an SCC, so we do kind inference for
-- everything.  Need an explicit type signature.
data K (a :: k) = K
data S (p :: forall k. k -> Type) = S (SSyn K)
type SSyn = (S :: (forall k. k -> Type) -> Type)
