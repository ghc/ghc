module Var where

import GhcPrelude
import Outputable
import Data.Data

type Id = Var
data Var

instance Eq Var
instance Ord Var
instance Data Var
instance Outputable Var
