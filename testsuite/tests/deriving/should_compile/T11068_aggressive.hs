{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -finline-generics-aggressively #-}
module T11068_aggressive where

import GHC.Generics

-- For 2 data constructors -finline-generics annotates class methods of the
-- derived Generic instance as INLINE[1] only if each has at most 5 fields.
data X
  = X1 Int Int Int Int Int Int Int Int Int Int
  | X2 Int Int Int Int Int Int Int Int Int Int
  deriving Generic
