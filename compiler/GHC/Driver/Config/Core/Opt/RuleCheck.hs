module GHC.Driver.Config.Core.Opt.RuleCheck ( initRuleCheckOpts ) where

import GHC.Prelude

import GHC.Driver.Config.Core.Rules
import GHC.Driver.Session ( DynFlags )

import GHC.Core.Opt.RuleCheck ( RuleCheckOpts(..) )

import GHC.Types.Basic ( CompilerPhase )

initRuleCheckOpts :: DynFlags -> CompilerPhase -> String -> RuleCheckOpts
initRuleCheckOpts dflags phase pat = RuleCheckOpts
  { rc_phase = phase
  , rc_pattern = pat
  , rc_rule_opts = initRuleOpts dflags
  }
