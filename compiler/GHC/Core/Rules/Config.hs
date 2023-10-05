module GHC.Core.Rules.Config where

import GHC.Prelude
import GHC.Platform

-- | Rule options
data RuleOpts = RuleOpts
   { roPlatform                :: !Platform -- ^ Target platform
   , roNumConstantFolding      :: !Bool     -- ^ Enable more advanced numeric constant folding
   , roExcessRationalPrecision :: !Bool     -- ^ Cut down precision of Rational values to that of Float/Double if disabled
   , roBignumRules             :: !Bool     -- ^ Enable rules for bignums
   }

