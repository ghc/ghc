module GHC.CmmToLlvm.Version
  ( LlvmVersion(..)
  , supportedLlvmVersionLowerBound
  , supportedLlvmVersionUpperBound
  , parseLlvmVersion
  , llvmVersionSupported
  , llvmVersionStr
  , llvmVersionList
  )
where

import GHC.Prelude

import GHC.CmmToLlvm.Version.Type
import GHC.CmmToLlvm.Version.Bounds

import Data.Char (isDigit)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE

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

llvmVersionSupported :: LlvmVersion -> Bool
llvmVersionSupported v =
  v >= supportedLlvmVersionLowerBound && v < supportedLlvmVersionUpperBound

llvmVersionStr :: LlvmVersion -> String
llvmVersionStr = intercalate "." . map show . llvmVersionList

llvmVersionList :: LlvmVersion -> [Int]
llvmVersionList = NE.toList . llvmVersionNE
