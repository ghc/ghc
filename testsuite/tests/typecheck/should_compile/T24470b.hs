{-# OPTIONS_GHC -Wno-implicit-rhs-quantification #-}
{-# LANGUAGE TypeAbstractions #-}

module T24470b where

import Data.Kind
import Data.Data

type SynOK :: forall k. k -> Type
type SynOK @j = Proxy :: j -> Type
