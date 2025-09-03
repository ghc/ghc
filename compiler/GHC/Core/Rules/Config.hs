module GHC.Core.Rules.Config where

import GHC.Prelude
import GHC.Platform

-- | Rule options
data RuleOpts = RuleOpts
   { roPlatform                :: !Platform
     -- ^ Target platform
   , roNumConstantFolding      :: !Bool
     -- ^ Enable constant folding through nested expressions.
     --
     -- See Note [Constant folding through nested expressions] in GHC.Core.Opt.ConstantFold
   , roExcessRationalPrecision :: !Bool
     -- ^ Cut down precision of Rational values to that of Float/Double if disabled
   , roBignumRules             :: !Bool
     -- ^ Enable built-in bignum rules (requires roBuiltinRules to be True too)
     --
     -- See Note [Built-in bignum rules] in GHC.Core.Opt.ConstantFold
   , roBuiltinRules            :: !Bool
     -- ^ Enable or disable all builtin rules (including bignum rules)
   }

