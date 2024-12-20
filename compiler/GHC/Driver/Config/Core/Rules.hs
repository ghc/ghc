module GHC.Driver.Config.Core.Rules
  ( initRuleOpts
  ) where

import GHC.Prelude

import GHC.Driver.Flags
import GHC.Driver.DynFlags ( DynFlags, gopt, targetPlatform )

import GHC.Core.Rules.Config

-- | Initialize RuleOpts from DynFlags
initRuleOpts :: DynFlags -> RuleOpts
initRuleOpts dflags = RuleOpts
  { roPlatform                = targetPlatform dflags
  , roNumConstantFolding      = gopt Opt_NumConstantFolding dflags
  , roExcessRationalPrecision = gopt Opt_ExcessPrecision dflags
  , roBignumRules             = True
  }
