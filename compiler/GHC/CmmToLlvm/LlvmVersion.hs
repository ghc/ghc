{-# LANGUAGE CPP #-}

module GHC.CmmToLlvm.LlvmVersion where

#include "ghc-llvm-version.h"

import GHC.Prelude

import Data.Char (isDigit)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE

-- ----------------------------------------------------------------------------
-- * Llvm Version
--

newtype LlvmVersion = LlvmVersion { llvmVersionNE :: NE.NonEmpty Int }
  deriving (Eq, Ord)

parseLlvmVersion :: String -> Maybe LlvmVersion
parseLlvmVersion =
    fmap LlvmVersion . NE.nonEmpty . go [] . dropWhile (not . isDigit)
  where
    go vs s
      | null ver_str
      = reverse vs
      | '.' : rest' <- rest
      = go (read ver_str : vs) rest'
      | otherwise
      = reverse (read ver_str : vs)
      where
        (ver_str, rest) = span isDigit s

-- | The (inclusive) lower bound on the LLVM Version that is currently supported.
supportedLlvmVersionLowerBound :: LlvmVersion
supportedLlvmVersionLowerBound = LlvmVersion (sUPPORTED_LLVM_VERSION_MIN NE.:| [])

-- | The (not-inclusive) upper bound  bound on the LLVM Version that is currently supported.
supportedLlvmVersionUpperBound :: LlvmVersion
supportedLlvmVersionUpperBound = LlvmVersion (sUPPORTED_LLVM_VERSION_MAX NE.:| [])

llvmVersionSupported :: LlvmVersion -> Bool
llvmVersionSupported v =
  v >= supportedLlvmVersionLowerBound && v < supportedLlvmVersionUpperBound

llvmVersionStr :: LlvmVersion -> String
llvmVersionStr = intercalate "." . map show . llvmVersionList

llvmVersionList :: LlvmVersion -> [Int]
llvmVersionList = NE.toList . llvmVersionNE

