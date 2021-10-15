{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}

module T14561 where

import Data.Coerce
import GHC.Exts

badId :: forall r (a :: TYPE r). a -> a
badId = coerce
-- Un-saturated application of a representation-polymorphic
-- function that must be eta-expanded
