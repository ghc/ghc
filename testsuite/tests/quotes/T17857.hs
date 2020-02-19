{-# LANGUAGE DeriveDataTypeable #-}
module T17857 where

import Data.Data
import Language.Haskell.TH.Syntax

data T = MkT deriving Data
instance Lift T where
  lift = liftData
  liftTyped = unsafeTExpCoerce . lift
