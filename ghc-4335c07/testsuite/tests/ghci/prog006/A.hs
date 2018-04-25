module A where

import {-# source #-} Boot

class Class a where
  method :: a -> Data -> a
