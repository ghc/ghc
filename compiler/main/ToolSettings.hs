module ToolSettings
  ( ToolSettings (..)
  ) where

import GhcPrelude

import CliOption
import Fingerprint

-- | Settings for other executables GHC calls.
--
-- Probably should futher split down by phase, or split between
-- platform-specific and platform-agnostic.
data ToolSettings = ToolSettings
  { toolSettings_ldSupportsCompactUnwind :: Bool
  , toolSettings_ldSupportsBuildId       :: Bool
  , toolSettings_ldSupportsFilelist      :: Bool
  , toolSettings_ldIsGnuLd               :: Bool
  , toolSettings_ccSupportsNoPie         :: Bool

  -- commands for particular phases
  , toolSettings_pgm_L       :: String
  , toolSettings_pgm_P       :: (String, [Option])
  , toolSettings_pgm_F       :: String
  , toolSettings_pgm_c       :: String
  , toolSettings_pgm_a       :: (String, [Option])
  , toolSettings_pgm_l       :: (String, [Option])
  , toolSettings_pgm_lm      :: (String, [Option])
  , toolSettings_pgm_dll     :: (String, [Option])
  , toolSettings_pgm_T       :: String
  , toolSettings_pgm_windres :: String
  , toolSettings_pgm_libtool :: String
  , toolSettings_pgm_ar      :: String
  , toolSettings_pgm_otool   :: String
  , toolSettings_pgm_install_name_tool :: String
  , toolSettings_pgm_ranlib  :: String
  , -- | LLVM: opt llvm optimiser
    toolSettings_pgm_lo      :: (String, [Option])
  , -- | LLVM: llc static compiler
    toolSettings_pgm_lc      :: (String, [Option])
  , -- | LLVM: c compiler
    toolSettings_pgm_lcc     :: (String, [Option])
  , toolSettings_pgm_i       :: String

  -- options for particular phases
  , toolSettings_opt_L             :: [String]
  , toolSettings_opt_P             :: [String]
  , -- | cached Fingerprint of sOpt_P
    -- See Note [Repeated -optP hashing]
    toolSettings_opt_P_fingerprint :: Fingerprint
  , toolSettings_opt_F             :: [String]
  , toolSettings_opt_c             :: [String]
  , toolSettings_opt_cxx           :: [String]
  , toolSettings_opt_a             :: [String]
  , toolSettings_opt_l             :: [String]
  , toolSettings_opt_lm            :: [String]
  , toolSettings_opt_windres       :: [String]
  , -- | LLVM: llvm optimiser
    toolSettings_opt_lo            :: [String]
  , -- | LLVM: llc static compiler
    toolSettings_opt_lc            :: [String]
  , -- | LLVM: c compiler
    toolSettings_opt_lcc           :: [String]
  , -- | iserv options
    toolSettings_opt_i             :: [String]

  , toolSettings_extraGccViaCFlags :: [String]
  }
