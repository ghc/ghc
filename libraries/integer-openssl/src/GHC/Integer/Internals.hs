{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Integer.Internals
  ( Integer(S#, Bp#, Bn#) -- data constructors are used in GHC
  , BigNum(BN#) -- data constructor is used in GHC
  , module GHC.Integer
  ) where

import GHC.Integer
import GHC.Integer.Type
