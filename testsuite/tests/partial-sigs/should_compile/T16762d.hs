{-# LANGUAGE TypeFamilies, PolyKinds, ExplicitForAll #-}

module Foo where

import Data.Kind
import Data.Proxy


type family F (a::k) (b::k)

-- Tricky because we can't quantify the '_' variable outside.
-- See Note [Generalising in tcTyFamInstEqnGuts] in TyCl

type instance forall k (x::k). F x _  = Int
