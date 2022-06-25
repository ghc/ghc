module GHC.Driver.Config.Core.Opt.Specialise
  ( initSpecialiseOpts
  ) where

import GHC.Prelude

import GHC.Core ( RuleBase )
import GHC.Core.Opt.Specialise ( SpecialiseOpts (..) )

import GHC.Driver.Env ( HscEnv, hsc_dflags, hscEPS )

import GHC.Unit.External ( eps_rule_base )
import GHC.Unit.Module ( ModuleSet )

import GHC.Types.SrcLoc ( SrcSpan )

import GHC.Utils.Outputable ( PrintUnqualified )

initSpecialiseOpts :: HscEnv -> SrcSpan -> RuleBase -> Char -> PrintUnqualified -> ModuleSet -> IO SpecialiseOpts
initSpecialiseOpts hsc_env loc rule_base mask print_unqual vis_orphs = do
  eps <- hscEPS hsc_env
  return SpecialiseOpts
    { so_dflags = hsc_dflags hsc_env
    , so_external_rule_base = eps_rule_base eps
    , so_loc = loc
    , so_rule_base = rule_base
    , so_uniq_mask = mask
    , so_unqual = print_unqual
    , so_visible_orphan_mods = vis_orphs
    }
