{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeepSubsumption #-}
module Repro where

import GHC.Generics
import Data.Binary

data FFIType
  = FFIVoid
  deriving (Show, Generic, Binary)
