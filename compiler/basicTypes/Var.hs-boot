module Var where

import GhcPrelude
import Data.Data

type Id = Var
data Var

instance Eq Var
instance Ord Var
instance Data Var
