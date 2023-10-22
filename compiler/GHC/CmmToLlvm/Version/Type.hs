module GHC.CmmToLlvm.Version.Type
  ( LlvmVersion(..)
  )
where

import GHC.Prelude

import qualified Data.List.NonEmpty as NE

newtype LlvmVersion = LlvmVersion { llvmVersionNE :: NE.NonEmpty Int }
  deriving (Eq, Ord)
