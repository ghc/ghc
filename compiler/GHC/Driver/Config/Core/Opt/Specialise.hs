module GHC.Driver.Config.Core.Opt.Specialise ( initSpecialiseOpts ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Env

import GHC.Core.Opt.Specialise ( SpecialiseOpts(..) )

import GHC.Plugins.Monad

import GHC.Unit.External

initSpecialiseOpts :: CoreM SpecialiseOpts
initSpecialiseOpts = do
  dflags <- getDynFlags
  hsc_env <- getHscEnv
  eps <- liftIO $ hscEPS hsc_env
  loc <- getSrcSpanM
  rule_base <- getRuleBase
  mask <- getUniqMask
  unqual <- getPrintUnqualified
  vis_orphans <- getVisibleOrphanMods
  return SpecialiseOpts
    { so_dflags = dflags
    , so_external_rule_base = eps_rule_base eps
    , so_loc = loc
    , so_rule_base = rule_base
    , so_uniq_mask = mask
    , so_unqual = unqual
    , so_visible_orphan_mods = vis_orphans
    }
