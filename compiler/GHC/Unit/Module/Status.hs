{-# LANGUAGE LambdaCase #-}

module GHC.Unit.Module.Status
   ( HscBackendAction(..)
   , HscRecompStatus (..)
   , RecompLinkables (..)
   , RecompBytecodeLinkable (..)
   , emptyRecompLinkables
   , justBytecode
   , justObjects
   , bytecodeAndObjects
   , safeCastHomeModLinkable
   )
where

import GHC.Prelude

import GHC.Unit
import GHC.Unit.Home.ModInfo
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModIface

import GHC.Linker.Types ( Linkable, WholeCoreBindingsLinkable, linkableIsNativeCodeOnly )

import GHC.Utils.Fingerprint
import GHC.Utils.Outputable
import GHC.Utils.Panic

-- | Status of a module in incremental compilation
data HscRecompStatus
    -- | Nothing to do because code already exists.
    = HscUpToDate ModIface RecompLinkables
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

-- | Linkables produced by @hscRecompStatus@. Might contain serialized core
-- which can be turned into BCOs (or object files), or used by some other
-- backend. See Note [Interface Files with Core Definitions].
data RecompLinkables = RecompLinkables { recompLinkables_bytecode :: !RecompBytecodeLinkable
                                       , recompLinkables_object   :: !(Maybe Linkable) }

data RecompBytecodeLinkable
  = NormalLinkable !(Maybe Linkable)
  | WholeCoreBindingsLinkable !WholeCoreBindingsLinkable

instance Outputable HscRecompStatus where
  ppr HscUpToDate{} = text "HscUpToDate"
  ppr HscRecompNeeded{} = text "HscRecompNeeded"

instance Outputable HscBackendAction where
  ppr (HscUpdate mi) = text "Update:" <+> (ppr (mi_module mi))
  ppr (HscRecomp _ ml _mi _mf) = text "Recomp:" <+> ppr ml

instance Outputable RecompLinkables where
  ppr (RecompLinkables l1 l2) = ppr l1 $$ ppr l2

instance Outputable RecompBytecodeLinkable where
  ppr (NormalLinkable lm) = text "NormalLinkable:" <+> ppr lm
  ppr (WholeCoreBindingsLinkable lm) = text "WholeCoreBindingsLinkable:" <+> ppr lm

emptyRecompLinkables :: RecompLinkables
emptyRecompLinkables = RecompLinkables (NormalLinkable Nothing) Nothing

safeCastHomeModLinkable :: HomeModLinkable -> RecompLinkables
safeCastHomeModLinkable (HomeModLinkable bc o) = RecompLinkables (NormalLinkable bc) o

justBytecode :: Either Linkable WholeCoreBindingsLinkable -> RecompLinkables
justBytecode = \case
  Left lm ->
    assertPpr (not (linkableIsNativeCodeOnly lm)) (ppr lm)
      $ emptyRecompLinkables { recompLinkables_bytecode = NormalLinkable (Just lm) }
  Right lm -> emptyRecompLinkables { recompLinkables_bytecode = WholeCoreBindingsLinkable lm }

justObjects :: Linkable -> RecompLinkables
justObjects lm =
  assertPpr (linkableIsNativeCodeOnly lm) (ppr lm)
    $ emptyRecompLinkables { recompLinkables_object = Just lm }

bytecodeAndObjects :: Either Linkable WholeCoreBindingsLinkable -> Linkable -> RecompLinkables
bytecodeAndObjects either_bc o = case either_bc of
  Left bc ->
    assertPpr (not (linkableIsNativeCodeOnly bc) && linkableIsNativeCodeOnly o) (ppr bc $$ ppr o)
      $ RecompLinkables (NormalLinkable (Just bc)) (Just o)
  Right bc ->
    assertPpr (linkableIsNativeCodeOnly o) (ppr o)
      $ RecompLinkables (WholeCoreBindingsLinkable bc) (Just o)
