module GHC.CmmToLlvm.Version
  ( LlvmVersion(..)
  , parseLlvmVersion
  , llvmVersionStr
  , llvmVersionList
  )
where

import GHC.Prelude

import GHC.CmmToLlvm.Version.Type

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

llvmVersionStr :: LlvmVersion -> String
llvmVersionStr = intercalate "." . map show . llvmVersionList

llvmVersionList :: LlvmVersion -> [Int]
llvmVersionList = NE.toList . llvmVersionNE
