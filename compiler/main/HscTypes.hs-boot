module HscTypes where

import NameEnv
import Binary

data Dependencies
data Usage
type FixityEnv = NameEnv FixItem
data FixItem
data ForeignStubs
data Warnings
data CompleteMatch
data HpcInfo

instance Binary Dependencies
instance Binary Usage
instance Binary ForeignStubs
instance Binary Warnings
instance Binary CompleteMatch
instance Binary HpcInfo
instance Binary FixItem
