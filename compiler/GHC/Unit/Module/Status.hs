module GHC.Unit.Module.Status
   ( HscStatus (..)
   )
where

import GHC.Prelude

import GHC.Driver.Session

import GHC.Unit
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModDetails

import GHC.Utils.Fingerprint

-- | Status of a module compilation to machine code
data HscStatus
    -- | Nothing to do.
    = HscNotGeneratingCode ModIface ModDetails
    -- | Nothing to do because code already exists.
    | HscUpToDate ModIface ModDetails
    -- | Update boot file result.
    | HscUpdateBoot ModIface ModDetails
    -- | Generate signature file (backpack)
    | HscUpdateSig ModIface ModDetails
    -- | Recompile this module.
    | HscRecomp
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
        , hscs_iface_dflags :: !DynFlags
          -- ^ Generate final iface using this DynFlags.
          -- FIXME (osa): I don't understand why this is necessary, but I spent
          -- almost two days trying to figure this out and I couldn't .. perhaps
          -- someone who understands this code better will remove this later.
        }
