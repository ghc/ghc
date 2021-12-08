-- | Llvm code generator configuration
module GHC.CmmToLlvm.Config
  ( LCGConfig(..)
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

data LCGConfig = LCGConfig
  { lcgPlatform             :: !Platform     -- ^ Target platform
  , lcgContext              :: !SDocContext  -- ^ Context for LLVM code generation
  , lcgFillUndefWithGarbage :: !Bool         -- ^ Fill undefined literals with garbage values
  , lcgSplitSections        :: !Bool         -- ^ Split sections
  , lcgBmiVersion           :: Maybe BmiVersion  -- ^ (x86) BMI instructions
  , lcgLlvmVersion          :: Maybe LlvmVersion -- ^ version of Llvm we're using
  , lcgDoWarn               :: !Bool         -- ^ True ==> warn unsupported Llvm version
  , lcgPlatformMisc         :: !String       -- ^ mirror DynFlags platformMisc_llvmTarget
  , lcgLlvmConfig           :: LlvmConfig    -- ^ mirror DynFlags LlvmConfig NB. this field must be lazy
                                             -- see Note [LLVM Configuration] in "GHC.SysTools"
  }
