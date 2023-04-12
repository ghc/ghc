{-# OPTIONS_GHC -O1 #-}
{-# LANGUAGE DataKinds #-}

module T23146_lifted_unliftedA where

import Data.Kind
import Data.Type.Equality

data NP a where
  UNil :: {-# UNPACK #-} !(a :~: True) -> NP a
  (::*) :: Bool -> NP True -> NP True


