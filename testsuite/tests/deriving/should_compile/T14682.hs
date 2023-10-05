{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
module T14682 where

import Data.Data
import Data.Ix
import Language.Haskell.TH.Syntax

data Foo = Foo Int Int
  deriving (Show, Lift, Data, Eq, Ord, Ix)
