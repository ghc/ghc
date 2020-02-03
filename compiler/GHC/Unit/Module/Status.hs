module GHC.Unit.Module.Status
  ( HscFrontendStatus'(..)
  , HscMiddleStatus(..)
  ) where

import GHC.Prelude

import GHC.Unit
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModSummary

import GHC.Utils.Fingerprint

-- | Status of a compilation to an "tc interface", i.e. enough information to
-- begin type checking the next module

-- TODO Type parameter until we sort imports out
data HscFrontendStatus' tcGblEnv

    -- | Nothing to do because interface already exists.
    = HscFrontendStatus_UpToDate ModIface ModDetails

    -- | Something changed. For regular module recompile this module at least
    -- through desugaring. For hs-boot or hsig, update stub objects.
    | HscFrontendStatus_Recomp
        { hsc_frontend_status_simple_mod_iface :: ModIface
        , hsc_frontend_status_simple_mod_details :: ModDetails
        , hsc_frontend_status_mod_sumary :: ModSummary
        , hsc_frontend_status_tc_result :: tcGblEnv
        , hsc_frontend_status_mb_old_hash_ :: Maybe Fingerprint
        }

-- | Status of a compilation from type checked code to hard executable code. Not
-- coincidentally, this is the slow parts of compilation.
data HscMiddleStatus

    -- | Nothing to do. Already have frontend's mod iface and details.
    = HscMiddleStatus_NotGeneratingCode

    --- TODO add this back for fast path distinct from the above. E.g. we could
    --- have already typed checked a module but changed the optimization
    --- settings so the two conceptual interface files are not valid or invalid
    --- in lock step.

    --- -- | Nothing to do because code already exists.
    --- | HscMiddleStatus_UpToDate ModIface ModDetails

    -- | Recompile this module.
    | HscMiddleStatus_Recomp
        { hscs_guts           :: CgGuts
          -- ^ Information for the code generator.
        , hscs_mod_location   :: !ModLocation
          -- ^ Module info
        , hscs_mod_details    :: !ModDetails
        , hscs_partial_iface  :: !PartialModIface
          -- ^ Partial interface
        , hscs_old_iface_hash :: !(Maybe Fingerprint)
          -- ^ Old interface hash for this compilation, if an old interface file
          -- exists. Pass to `hscMaybeWriteIface` when writing the interface to
          -- avoid updating the existing interface when the interface isn't
          -- changed.
        }
