{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Goof where

import GHC.Exts (coerce)
import GHC.Types (RuntimeRep,TYPE,Coercible)

goof :: forall (rep :: RuntimeRep) (x :: TYPE rep) (y :: TYPE rep).
  Coercible x y => x -> y
goof = coerce
