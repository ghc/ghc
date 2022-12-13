module GHC.Iface.Recomp.Types ( ModIfaceSelfRecomp(..)
                              ) where

import GHC.Prelude
import GHC.Fingerprint
import GHC.Utils.Outputable
import GHC.Unit.Module.Deps

import GHC.Utils.Binary

import Control.DeepSeq

{-
Note [Self recompilation information in interface files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The flag -fwrite-if-self-recomp controls whether
interface files contain the information necessary to answer the
question:

  Is the interface file up-to-date, relative to:
    * the source file it corresponds to,
    * the flags passed to the GHC invocation to compile it,
    * its dependencies (e.g. imported items, watched files added by addDependentFile, ...)

If there is no self-recompilation information stored, then we always re-generate
the interface file from scratch.

Why? Most packages are only built once either by a distribution or cabal
and then placed into an immutable store, after which we will never ask
this question. Therefore we can derive two benefits from omitting this
information.

* Primary motivation: It vastly reduces the surface area for creating
  non-deterministic interface files. See issue #10424 which motivated a
  proper fix to that issue. Distributions have long contained versions
  of GHC which just have broken self-recompilation checking (in order to
  get deterministic interface files).

* Secondary motivation: This reduces the size of interface files
  slightly.. the `mi_usages` field can be quite big but probably this
  isn't such a great benefit.

* Third motivation: Conceptually clarity about which parts of an
  interface file are used in order to **communicate** with subsequent
  packages about the **interface** for a module. And which parts are
  used to self-communicate during recompilation checking.

The main tracking issue is #22188 but fixes issues such as #10424 in a
proper way.

-}

-- | The information for a module which is only used when deciding whether to recompile
-- itself.
--
-- See Note [Self recompilation information in interface files]
data ModIfaceSelfRecomp =
    ModIfaceSelfRecomp { mi_sr_src_hash :: !Fingerprint
                       -- ^ Hash of the .hs source, used for recompilation checking.
                       , mi_sr_usages   :: [Usage]
                       -- ^ Usages; kept sorted so that it's easy to decide
                       -- whether to write a new iface file (changing usages
                       -- doesn't affect the hash of this module)
                       -- NOT STRICT!  we read this field lazily from the interface file
                       -- It is *only* consulted by the recompilation checker

                       , mi_sr_flag_hash :: !Fingerprint
                       -- ^ Hash of the important flags used when compiling the module, excluding
                       -- optimisation flags
                       , mi_sr_opt_hash :: !Fingerprint
                       -- ^ Hash of optimisation flags
                       , mi_sr_hpc_hash :: !Fingerprint
                       -- ^ Hash of hpc flags
                       , mi_sr_plugin_hash :: !Fingerprint
                       -- ^ Hash of plugins
                       }


instance Binary ModIfaceSelfRecomp where
  put_ bh (ModIfaceSelfRecomp{mi_sr_src_hash, mi_sr_usages, mi_sr_flag_hash, mi_sr_opt_hash, mi_sr_hpc_hash, mi_sr_plugin_hash}) = do
    put_ bh mi_sr_src_hash
    lazyPut bh mi_sr_usages
    put_ bh mi_sr_flag_hash
    put_ bh mi_sr_opt_hash
    put_ bh mi_sr_hpc_hash
    put_ bh mi_sr_plugin_hash

  get bh = do
    src_hash    <- get bh
    usages      <- lazyGet bh
    flag_hash   <- get bh
    opt_hash    <- get bh
    hpc_hash    <- get bh
    plugin_hash <- get bh
    return $ ModIfaceSelfRecomp { mi_sr_src_hash = src_hash, mi_sr_usages = usages, mi_sr_flag_hash = flag_hash, mi_sr_opt_hash = opt_hash, mi_sr_hpc_hash = hpc_hash, mi_sr_plugin_hash = plugin_hash }

instance Outputable ModIfaceSelfRecomp where
  ppr (ModIfaceSelfRecomp{mi_sr_src_hash, mi_sr_usages, mi_sr_flag_hash, mi_sr_opt_hash, mi_sr_hpc_hash, mi_sr_plugin_hash})
    = vcat [text "Self-Recomp"
            , nest 2 (vcat [ text "src hash:" <+> ppr mi_sr_src_hash
                           , text "usages:" <+> ppr (length mi_sr_usages)
                           , text "flag hash:" <+> ppr mi_sr_flag_hash
                           , text "opt hash:" <+> ppr mi_sr_opt_hash
                           , text "hpc hash:" <+> ppr mi_sr_hpc_hash
                           , text "plugin hash:" <+> ppr mi_sr_plugin_hash
                           ])]

instance NFData ModIfaceSelfRecomp where
  -- Note (MP): does not deeply force Usages but the old ModIface logic didn't either, so
  -- I left it as a shallow force.
  rnf (ModIfaceSelfRecomp src_hash usages flag_hash opt_hash hpc_hash plugin_hash)
    = src_hash `seq` usages `seq` flag_hash `seq` opt_hash `seq` hpc_hash `seq` plugin_hash `seq` ()