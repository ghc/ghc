module GHC.Unit.Module.Status
   ( HscBackendAction(..), HscRecompStatus (..)
   )
where

import GHC.Prelude

import GHC.Unit
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModIface

import GHC.Utils.Fingerprint
import GHC.Linker.Types
import GHC.Utils.Outputable

-- | Status of a module in incremental compilation
data HscRecompStatus
    -- | Nothing to do because code already exists.
    = HscUpToDate ModIface (Maybe Linkable)
    -- | Recompilation of module, or update of interface is required. Optionally
    -- pass the old interface hash to avoid updating the existing interface when
    -- it has not changed.
    | HscRecompNeeded (Maybe Fingerprint)

-- | Action to perform in backend compilation
data HscBackendAction
    -- | Update the boot and signature file results.
    = HscUpdate ModIface
    -- | Recompile this module.
    | HscRecomp
        { hscs_guts           :: CgGuts
          -- ^ Information for the code generator.
        , hscs_mod_location   :: !ModLocation
          -- ^ Module info
        , hscs_partial_iface  :: !PartialModIface
          -- ^ Partial interface
        , hscs_old_iface_hash :: !(Maybe Fingerprint)
          -- ^ Old interface hash for this compilation, if an old interface file
          -- exists. Pass to `hscMaybeWriteIface` when writing the interface to
          -- avoid updating the existing interface when the interface isn't
          -- changed.
        }


instance Outputable HscBackendAction where
  ppr (HscUpdate mi) = text "Update:" <+> (ppr (mi_module mi))
  ppr (HscRecomp _ ml _mi _mf) = text "Recomp:" <+> ppr ml
