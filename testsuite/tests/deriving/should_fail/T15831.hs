{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PolyKinds #-}
module T15831 where

import Data.Functor.Const (Const(..))
import GHC.Exts (Any)

newtype Age = MkAge Int
  deriving Eq
    via Const Int Any
