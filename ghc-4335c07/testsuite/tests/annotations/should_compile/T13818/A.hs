{-# LANGUAGE DeriveDataTypeable #-}
module A where

import Data.Typeable
import Data.Data

data FromA = FromA
  deriving (Typeable, Data)
