module GHC.Driver.Config.Core.Opt.Specialise
  ( initSpecialiseOpts
  ) where

import GHC.Prelude

import GHC.Core.Opt.Specialise ( SpecialiseOpts (..) )

import GHC.Driver.Config ( initSimpleOpts )
import GHC.Driver.Config.Core.Rules ( initRuleOpts )
import GHC.Driver.Config.Diagnostic ( initDiagOpts )
import GHC.Driver.Session ( DynFlags, DiagnosticReason(..), GeneralFlag(..), WarningFlag(..), gopt, initSDocContext, wopt )

import GHC.Utils.Error ( mkMCDiagnostic )
import GHC.Utils.Outputable ( PrintUnqualified, defaultUserStyle )

initSpecialiseOpts :: DynFlags -> Char -> PrintUnqualified -> SpecialiseOpts
initSpecialiseOpts dflags mask print_unqual = SpecialiseOpts
  { so_uniq_mask = mask
  , so_unqual = print_unqual
  , so_cross_module_specialise = gopt Opt_CrossModuleSpecialise dflags
  , so_specialise_aggressively = gopt Opt_SpecialiseAggressively dflags
  , so_warn_missed_specs = warn_missed_specs
  , so_warn_all_missed_specs = warn_all_missed_specs
  , so_sdoc_context = initSDocContext dflags defaultUserStyle
  , so_simpl_opts = initSimpleOpts dflags
  , so_rule_opts = initRuleOpts dflags
  }
  where
    diag_opts = initDiagOpts dflags
    warn_missed_specs
      | wopt Opt_WarnMissedSpecs dflags = Just $ mkMCDiagnostic diag_opts (WarningWithFlag Opt_WarnMissedSpecs)
      | otherwise = Nothing
    warn_all_missed_specs
      | wopt Opt_WarnAllMissedSpecs dflags = Just $ mkMCDiagnostic diag_opts (WarningWithFlag Opt_WarnAllMissedSpecs)
      | otherwise = Nothing
