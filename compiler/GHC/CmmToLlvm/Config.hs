{-# LANGUAGE CPP #-}

-- | Llvm code generator configuration
module GHC.CmmToLlvm.Config
  ( LlvmCgConfig(..)
  , LlvmConfig(..)
  , LlvmTarget(..)
  , initLlvmConfig
  -- * LLVM version
  , LlvmVersion(..)
  , supportedLlvmVersionLowerBound
  , supportedLlvmVersionUpperBound
  , parseLlvmVersion
  , llvmVersionSupported
  , llvmVersionStr
  , llvmVersionList
  )
where

#include "ghc-llvm-version.h"

import GHC.Prelude
import GHC.Platform

import GHC.Utils.Outputable
import GHC.Settings.Utils
import GHC.Utils.Panic

import Data.Char (isDigit)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import System.FilePath

data LlvmCgConfig = LlvmCgConfig
  { llvmCgPlatform          :: !Platform     -- ^ Target platform
  , llvmCgContext           :: !SDocContext  -- ^ Context for LLVM code generation
  , llvmCgFillUndefWithGarbage :: !Bool      -- ^ Fill undefined literals with garbage values
  , llvmCgSplitSection      :: !Bool         -- ^ Split sections
  , llvmCgBmiVersion        :: Maybe BmiVersion  -- ^ (x86) BMI instructions
  , llvmCgLlvmVersion       :: Maybe LlvmVersion -- ^ version of Llvm we're using
  , llvmCgDoWarn            :: !Bool         -- ^ True ==> warn unsupported Llvm version
  , llvmCgLlvmTarget        :: !String       -- ^ target triple passed to LLVM
  , llvmCgLlvmConfig        :: !LlvmConfig   -- ^ Supported LLVM configurations.
                                             -- see Note [LLVM configuration]
  }

data LlvmTarget = LlvmTarget
  { lDataLayout :: String
  , lCPU        :: String
  , lAttributes :: [String]
  }

-- Note [LLVM configuration]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- The `llvm-targets` and `llvm-passes` files are shipped with GHC and contain
-- information needed by the LLVM backend to invoke `llc` and `opt`.
-- Specifically:
--
--  * llvm-targets maps autoconf host triples to the corresponding LLVM
--    `data-layout` declarations. This information is extracted from clang using
--    the script in utils/llvm-targets/gen-data-layout.sh and should be updated
--    whenever we target a new version of LLVM.
--
--  * llvm-passes maps GHC optimization levels to sets of LLVM optimization
--    flags that GHC should pass to `opt`.
--
-- This information is contained in files rather the GHC source to allow users
-- to add new targets to GHC without having to recompile the compiler.
--

initLlvmConfig :: FilePath -> IO LlvmConfig
initLlvmConfig top_dir
  = do
      targets <- readAndParse "llvm-targets"
      passes <- readAndParse "llvm-passes"
      return $ LlvmConfig
        { llvmTargets = fmap mkLlvmTarget <$> targets
        , llvmPasses = passes
        }
  where
    readAndParse :: Read a => String -> IO a
    readAndParse name = do
      let f = top_dir </> name
      llvmConfigStr <- readFile f
      case maybeReadFuzzy llvmConfigStr of
        Just s -> return s
        Nothing -> pgmError ("Can't parse LLVM config file: " ++ show f)

    mkLlvmTarget :: (String, String, String) -> LlvmTarget
    mkLlvmTarget (dl, cpu, attrs) = LlvmTarget dl cpu (words attrs)

data LlvmConfig = LlvmConfig
  { llvmTargets :: [(String, LlvmTarget)]
  , llvmPasses  :: [(Int, String)]
  }


---------------------------------------------------------
-- LLVM version
---------------------------------------------------------

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
