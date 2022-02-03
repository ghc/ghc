-- | Llvm code generator configuration
module GHC.CmmToLlvm.Config
  ( LlvmCgConfig(..)
  , LlvmVersion(..)
  )
where

import GHC.Prelude
import GHC.Platform

import GHC.Utils.Outputable
import GHC.Driver.Session

import qualified Data.List.NonEmpty as NE

newtype LlvmVersion = LlvmVersion { llvmVersionNE :: NE.NonEmpty Int }
  deriving (Eq, Ord)

data LlvmCgConfig = LlvmCgConfig
  { llvmCgPlatform          :: !Platform     -- ^ Target platform
  , llvmCgContext           :: !SDocContext  -- ^ Context for LLVM code generation
  , llvmCgFillUndefWithGarbage :: !Bool      -- ^ Fill undefined literals with garbage values
  , llvmCgSplitSection      :: !Bool         -- ^ Split sections
  , llvmCgBmiVersion        :: Maybe BmiVersion  -- ^ (x86) BMI instructions
  , llvmCgLlvmVersion       :: Maybe LlvmVersion -- ^ version of Llvm we're using
  , llvmCgDoWarn            :: !Bool         -- ^ True ==> warn unsupported Llvm version
  , llvmCgLlvmTarget        :: !String       -- ^ target triple passed to LLVM
  , llvmCgLlvmConfig        :: !LlvmConfig   -- ^ mirror DynFlags LlvmConfig.
    -- see Note [LLVM configuration] in "GHC.SysTools". This can be strict since
    -- GHC.Driver.Config.CmmToLlvm.initLlvmCgConfig verifies the files are present.
  }
