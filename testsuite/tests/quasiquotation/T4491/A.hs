{-# LANGUAGE DeriveDataTypeable #-}

module A where

import Data.Data
import Data.Typeable

data Foo = Foo Int
  deriving (Show, Data, Typeable)
