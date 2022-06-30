module GHC.Driver.Config.Core.Opt.Specialise
  ( initSpecialiseOpts
  ) where

import GHC.Prelude

import GHC.Core ( RuleBase )
import GHC.Core.Opt.Specialise ( SpecialiseOpts (..) )

import GHC.Driver.Config ( initSimpleOpts )
import GHC.Driver.Config.Core.Rules ( initRuleOpts )
import GHC.Driver.Config.Diagnostic ( initDiagOpts )
import GHC.Driver.Env ( HscEnv, hsc_dflags, hscEPS )
import GHC.Driver.Session ( DiagnosticReason(..), GeneralFlag(..), WarningFlag(..), gopt, initSDocContext, wopt )

import GHC.Unit.External ( eps_rule_base )
import GHC.Unit.Module ( ModuleSet )

import GHC.Types.SrcLoc ( SrcSpan )

import GHC.Utils.Error ( mkMCDiagnostic )
import GHC.Utils.Outputable ( PrintUnqualified, defaultUserStyle )

initSpecialiseOpts :: HscEnv -> SrcSpan -> RuleBase -> Char -> PrintUnqualified -> ModuleSet -> IO SpecialiseOpts
initSpecialiseOpts hsc_env loc rule_base mask print_unqual vis_orphs = do
  eps <- hscEPS hsc_env
  return SpecialiseOpts
    { so_external_rule_base = eps_rule_base eps
    , so_loc = loc
    , so_rule_base = rule_base
    , so_uniq_mask = mask
    , so_unqual = print_unqual
    , so_visible_orphan_mods = vis_orphs
    , so_cross_module_specialise = gopt Opt_CrossModuleSpecialise dflags
    , so_specialise_aggressively = gopt Opt_SpecialiseAggressively dflags
    , so_warn_missed_specs = warn_missed_specs
    , so_warn_all_missed_specs = warn_all_missed_specs
    , so_sdoc_context = initSDocContext dflags defaultUserStyle
    , so_simpl_opts = initSimpleOpts dflags
    , so_rule_opts = initRuleOpts dflags
    }
  where
    dflags = hsc_dflags hsc_env
    diag_opts = initDiagOpts dflags
    warn_missed_specs
      | wopt Opt_WarnMissedSpecs dflags = Just $ mkMCDiagnostic diag_opts (WarningWithFlag Opt_WarnMissedSpecs)
      | otherwise = Nothing
    warn_all_missed_specs
      | wopt Opt_WarnAllMissedSpecs dflags = Just $ mkMCDiagnostic diag_opts (WarningWithFlag Opt_WarnAllMissedSpecs)
      | otherwise = Nothing
